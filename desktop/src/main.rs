// Prevents additional console window on Windows in release
#![cfg_attr(
    all(not(debug_assertions), target_os = "windows"),
    windows_subsystem = "windows"
)]

use std::env;
use std::net::TcpListener;
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;
use tauri::Manager;

/// Find an available port in the given range
fn find_available_port(start: u16, end: u16) -> Option<u16> {
    for port in start..=end {
        if TcpListener::bind(("127.0.0.1", port)).is_ok() {
            return Some(port);
        }
    }
    None
}

/// Get the binary name for the current platform
/// On Windows, we look for both fplca.exe and fplca (for bundled resources)
fn get_binary_name() -> &'static str {
    "fplca"
}

/// Get the binary path, checking for both fplca and fplca.exe on Windows
fn get_binary_path(resource_dir: &PathBuf) -> PathBuf {
    let base = resource_dir.join("fplca");

    #[cfg(windows)]
    {
        // On Windows, try fplca.exe first, then fplca
        let exe_path = resource_dir.join("fplca.exe");
        if exe_path.exists() {
            return exe_path;
        }
    }

    base
}

/// Get the resource directory for bundled files (used before Tauri app is initialized)
fn get_resource_dir_fallback() -> PathBuf {
    let binary_name = get_binary_name();

    // Check for installed location first
    #[cfg(target_os = "linux")]
    {
        // Debian package installs to /usr/lib/fplca
        let installed_path = PathBuf::from("/usr/lib/fplca");
        if installed_path.join(binary_name).exists() {
            return installed_path;
        }
    }

    #[cfg(target_os = "windows")]
    {
        // Windows installer typically installs to Program Files
        if let Ok(program_files) = env::var("ProgramFiles") {
            let installed_path = PathBuf::from(program_files).join("fplca");
            // Check for both fplca.exe and fplca
            if installed_path.join("fplca.exe").exists() || installed_path.join(binary_name).exists() {
                return installed_path;
            }
        }
    }

    let exe_dir = env::current_exe()
        .ok()
        .and_then(|p| p.parent().map(|p| p.to_path_buf()))
        .unwrap_or_else(|| PathBuf::from("."));

    // In development, try multiple locations
    let candidates = [
        exe_dir.join("resources"),
        PathBuf::from("resources"),
        PathBuf::from("desktop/resources"),
        exe_dir.clone(),
    ];

    for candidate in &candidates {
        if candidate.join(binary_name).exists() {
            return candidate.clone();
        }
        #[cfg(windows)]
        if candidate.join("fplca.exe").exists() {
            return candidate.clone();
        }
    }

    exe_dir
}

/// Get the environment variable name for library path on current platform
fn get_library_path_env_var() -> &'static str {
    if cfg!(windows) {
        "PATH"
    } else {
        "LD_LIBRARY_PATH"
    }
}

/// Get the path separator for the current platform
fn get_path_separator() -> &'static str {
    if cfg!(windows) {
        ";"
    } else {
        ":"
    }
}

/// Build library path including bundled libraries (LD_LIBRARY_PATH on Unix, PATH on Windows)
fn build_library_path(resource_dir: &PathBuf) -> String {
    let lib_dir = resource_dir.join("lib");
    let lib_path = lib_dir.to_string_lossy().to_string();
    let env_var = get_library_path_env_var();
    let separator = get_path_separator();

    // Prepend to existing path if any
    match env::var(env_var) {
        Ok(existing) if !existing.is_empty() => format!("{}{}{}", lib_path, separator, existing),
        _ => lib_path,
    }
}

/// Wait for the backend to be ready by polling the health endpoint
async fn wait_for_backend(port: u16, timeout_secs: u64) -> bool {
    let url = format!("http://127.0.0.1:{}/api/v1/databases", port);
    let client = reqwest::Client::builder()
        .timeout(Duration::from_secs(2))
        .build()
        .unwrap();

    let start = std::time::Instant::now();
    let timeout = Duration::from_secs(timeout_secs);

    while start.elapsed() < timeout {
        match client.get(&url).send().await {
            Ok(response) if response.status().is_success() => {
                return true;
            }
            _ => {
                tokio::time::sleep(Duration::from_millis(500)).await;
            }
        }
    }

    false
}

/// Spawn the fplca backend process
fn spawn_backend(resource_dir: &PathBuf, port: u16) -> Result<Child, String> {
    let fplca_binary = get_binary_path(resource_dir);
    let web_dir = resource_dir.join("web");
    let lib_path = build_library_path(resource_dir);
    let lib_env_var = get_library_path_env_var();

    // Verify binary exists
    if !fplca_binary.exists() {
        return Err(format!(
            "fplca binary not found at: {}",
            fplca_binary.display()
        ));
    }

    // Build command with desktop mode flags
    // Use --config for BYOL mode (no database required at startup)
    let config_file = resource_dir.join("fplca.toml");
    let mut cmd = Command::new(&fplca_binary);
    cmd.env(lib_env_var, &lib_path);

    // Use config file if it exists (enables BYOL mode)
    if config_file.exists() {
        cmd.arg("--config").arg(&config_file);
    }

    // Note: "server" subcommand must come first, then its options
    cmd.arg("server")
        .arg("--desktop")
        .arg("--port")
        .arg(port.to_string())
        .arg("--static-dir")
        .arg(&web_dir)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    // Set PETSC_OPTIONS for optimal solver behavior
    cmd.env(
        "PETSC_OPTIONS",
        "-mat_mumps_icntl_14 80 -mat_mumps_icntl_24 1",
    );

    cmd.spawn()
        .map_err(|e| format!("Failed to spawn fplca backend: {}", e))
}

struct BackendState {
    process: Option<Child>,
}

fn main() {
    // Find available port
    let port = find_available_port(8080, 8100)
        .expect("No available port found in range 8080-8100");

    println!("Using port: {}", port);

    // Shared state for backend process
    let backend_state = Arc::new(Mutex::new(BackendState {
        process: None,
    }));
    let backend_state_clone = Arc::clone(&backend_state);

    // Flag to track if backend is ready
    let backend_ready = Arc::new(AtomicBool::new(false));
    let backend_ready_clone = Arc::clone(&backend_ready);

    // Build and run Tauri app
    tauri::Builder::default()
        .plugin(tauri_plugin_shell::init())
        .setup(move |app| {
            // Get resource directory from Tauri - this is the correct path for bundled resources
            let resource_dir = app.path().resource_dir()
                .unwrap_or_else(|_| get_resource_dir_fallback());

            println!("Resource directory: {}", resource_dir.display());

            // List contents of resource dir for debugging
            if let Ok(entries) = std::fs::read_dir(&resource_dir) {
                println!("Resource directory contents:");
                for entry in entries.flatten() {
                    println!("  {:?}", entry.path());
                }
            }

            let backend_state = Arc::clone(&backend_state_clone);
            let backend_ready = Arc::clone(&backend_ready_clone);
            let main_window = app.get_webview_window("main").unwrap();

            // Spawn backend and wait for it to be ready
            std::thread::spawn(move || {
                // Start the backend
                match spawn_backend(&resource_dir, port) {
                    Ok(mut child) => {
                        // Capture and print backend stdout/stderr for debugging
                        if let Some(stdout) = child.stdout.take() {
                            std::thread::spawn(move || {
                                use std::io::{BufRead, BufReader};
                                let reader = BufReader::new(stdout);
                                for line in reader.lines().flatten() {
                                    println!("[backend stdout] {}", line);
                                }
                            });
                        }
                        if let Some(stderr) = child.stderr.take() {
                            std::thread::spawn(move || {
                                use std::io::{BufRead, BufReader};
                                let reader = BufReader::new(stderr);
                                for line in reader.lines().flatten() {
                                    eprintln!("[backend stderr] {}", line);
                                }
                            });
                        }

                        {
                            let mut state = backend_state.lock().unwrap();
                            state.process = Some(child);
                        }
                        println!("Backend started on port {}", port);

                        // Wait for backend to be ready using tokio runtime
                        let rt = tokio::runtime::Runtime::new().unwrap();
                        let ready = rt.block_on(wait_for_backend(port, 120));

                        if ready {
                            println!("Backend is ready");
                            backend_ready.store(true, Ordering::SeqCst);

                            // Navigate main window to the backend URL
                            let url = format!("http://127.0.0.1:{}/", port);
                            let _ = main_window.eval(&format!("window.location.href = '{}'", url));
                        } else {
                            eprintln!("Backend failed to start within timeout");
                        }
                    }
                    Err(e) => {
                        eprintln!("Failed to start backend: {}", e);
                    }
                }
            });

            Ok(())
        })
        .on_window_event(move |_window, event| {
            if let tauri::WindowEvent::CloseRequested { .. } = event {
                // Kill backend process on window close
                let mut state = backend_state.lock().unwrap();
                if let Some(ref mut child) = state.process {
                    println!("Shutting down backend...");
                    let _ = child.kill();
                    let _ = child.wait();
                    println!("Backend stopped");
                }
            }
        })
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
