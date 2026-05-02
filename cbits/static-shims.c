/* Compatibility shims for fully-static linking against ghcup's GHC 9.6
 * unix-2.8.6.0 archive on glibc >= 2.32.
 *
 * The unix package's static .o files were compiled against older glibc
 * headers that inlined `mknod()` / `mknodat()` to `__xmknod()` /
 * `__xmknodat()`. glibc 2.32 removed those internal aliases (they're
 * GLIBC_2.0 / GLIBC_2.1 only) and now exports `mknod` directly. The
 * dynamic shipped in any modern Linux still resolves them via the
 * versioned linker map, but a static link has no map to consult — the
 * symbols are simply missing.
 *
 * Forwarding to the modern public API restores the link without
 * affecting behaviour. Same trick used by Rust's musl tooling and
 * Nixpkgs' GHC static profile.
 */

#define _GNU_SOURCE
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>

int __xmknod(int ver, const char *path, mode_t mode, dev_t *dev) {
    (void)ver;
    return mknod(path, mode, *dev);
}

int __xmknodat(int ver, int dirfd, const char *path, mode_t mode, dev_t *dev) {
    (void)ver;
    return mknodat(dirfd, path, mode, *dev);
}
