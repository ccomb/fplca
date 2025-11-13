#!/bin/bash

# Expected Results for SAMPLE Data Validation
# Defines the correct inventory values for critical test cases

# Function to validate inventory results
validate_inventory() {
    local test_name="$1"
    local dataset="$2"
    local activity_uuid="$3"
    local flow_name="$4"
    local expected_amount="$5"
    local expected_unit="$6"
    local tolerance_pct="${7:-5}"  # Default 5% tolerance

    echo "  Validating: $flow_name in $test_name"

    # Get actual result (use awk instead of cut to handle quoted CSV fields properly)
    local result=$(env LD_LIBRARY_PATH="/home/dadafkas/projets/fpLCA/petsc/arch-linux-c-debug/lib:/home/dadafkas/projets/fpLCA/slepc/arch-linux-c-debug/lib" \
                  timeout 30s ./run.sh --data "$dataset" inventory "$activity_uuid" --no-cache --format csv --jsonpath 'ieFlows' 2>/dev/null | \
                  grep "$flow_name" | head -1 | awk -F',' '{print $(NF-1)}' 2>/dev/null || echo "")

    if [ -z "$result" ]; then
        echo "    ❌ FAILED - Flow '$flow_name' not found in inventory"
        return 1
    fi

    # Convert to comparable numbers (handle scientific notation)
    local actual=$(echo "$result" | awk '{printf "%.10f", $1}')
    local expected=$(echo "$expected_amount" | awk '{printf "%.10f", $1}')

    # Calculate percentage difference
    local diff_pct=$(echo "$actual $expected" | awk '{
        if ($2 == 0) {
            if ($1 == 0) print 0
            else print 100
        } else {
            print (($1 - $2) / $2) * 100
        }
    }')

    # Check if within tolerance
    local within_tolerance=$(echo "$diff_pct $tolerance_pct" | awk '{print (($1 < 0 ? -$1 : $1) <= $2) ? 1 : 0}')

    if [ "$within_tolerance" -eq 1 ]; then
        echo "    ✅ PASSED - $actual $expected_unit (expected: $expected_amount, diff: ${diff_pct}%)"
        return 0
    else
        echo "    ❌ FAILED - $actual $expected_unit (expected: $expected_amount, diff: ${diff_pct}%)"
        return 1
    fi
}

# Critical Test Cases with Expected Results

test_nuclear_power_units() {
    echo "🧪 Nuclear Power Plant - Activity Unit Conversions"
    local activity="nuclear-power-uuid_nuclear-electricity-uuid"
    local dataset="SAMPLE.units"

    # Test the fixed Radon-222 calculation (our critical regression test)
    validate_inventory "Nuclear-Radon" "$dataset" "$activity" "Radon-222" "1.0e-6" "kBq" 1

    # Test Cesium-137 (Bq to kBq conversion check)
    validate_inventory "Nuclear-Cesium" "$dataset" "$activity" "Cesium-137" "50.0" "Bq" 1

    return $?
}

test_steel_mass_conversions() {
    echo "🧪 Steel Production - Mass Unit Conversions"
    local activity="steel-prod-uuid_steel-uuid"
    local dataset="SAMPLE.units"

    local failed=0

    # Test CO2 emissions (mass unit conversions)
    # Activity produces 1 tonne steel with 2000.0 kg CO2 emissions
    if ! validate_inventory "Steel-CO2" "$dataset" "$activity" "Carbon dioxide, fossil" "2000.0" "kg" 2; then
        failed=1
    fi

    # Test particulates (g units - small mass conversion test)
    # Raw data: 50000.0 g particulates for 1 tonne steel
    if ! validate_inventory "Steel-Particulates" "$dataset" "$activity" "Particulates, > 10 um" "50000.0" "g" 5; then
        failed=1
    fi

    return $failed
}

test_refinery_joint_production() {
    echo "🧪 Oil Refinery - Joint Production Allocation"
    local activity="11111111-2222-3333-4444-555555555555_gasoline-uuid"
    local dataset="SAMPLE.multiproduct"

    local failed=0

    # Test CO2 emissions per functional unit (1L gasoline = reference product)
    # Direct emissions: 8.0 kg CO2 for 40.0 L gasoline → 0.2 kg CO2/L
    if ! validate_inventory "Refinery-CO2" "$dataset" "$activity" "Carbon dioxide, fossil" "0.2" "kg" 5; then
        failed=1
    fi

    # Test SO2 emissions per functional unit (1L gasoline)
    # Direct emissions: 0.1 kg SO2 for 40.0 L gasoline → 0.0025 kg SO2/L
    if ! validate_inventory "Refinery-SO2" "$dataset" "$activity" "Sulfur dioxide" "0.0025" "kg" 5; then
        failed=1
    fi

    return $failed
}

test_process_switching() {
    echo "🧪 Process Switching - Same UUID, Different Reference Products"
    local failed=0

    # Test Chemical Plant Mode B (ProcessId format)
    local chemical_activity="22222222-3333-4444-5555-666666666661_chemical-b-uuid"
    local switching_dataset="SAMPLE.switching"

    # Based on testing, system loads Mode B (Chemical B primary)
    if ! validate_inventory "Switching-CO2" "$switching_dataset" "$chemical_activity" "Carbon dioxide, fossil" "1.2" "kg" 5; then
        failed=1
    fi

    if ! validate_inventory "Switching-VOC" "$switching_dataset" "$chemical_activity" "Chemical B VOC emissions" "8.0" "g" 5; then
        failed=1
    fi

    # Note: Chemical A is a zero technosphere co-product, correctly filtered from biosphere inventory
    # No need to test it in inventory validation as it only shows biosphere flows

    # Test Power Plant Gas Mode (ProcessId format)
    local power_activity="33333333-4444-5555-6666-777777777772_gas-power-uuid"

    # Test gas mode CO2 emissions
    if ! validate_inventory "Power-CO2" "$switching_dataset" "$power_activity" "Carbon dioxide, fossil" "0.20" "kg" 5; then
        failed=1
    fi

    return $failed
}

# Run all validation tests
run_inventory_validation() {
    echo "=== Inventory Correctness Validation ==="
    echo "Testing actual computation results against expected values"
    echo

    local total_tests=0
    local passed_tests=0

    # Run test suites
    if test_nuclear_power_units; then
        passed_tests=$((passed_tests + 1))
    fi
    total_tests=$((total_tests + 1))
    echo

    if test_steel_mass_conversions; then
        passed_tests=$((passed_tests + 1))
    fi
    total_tests=$((total_tests + 1))
    echo

    if test_refinery_joint_production; then
        passed_tests=$((passed_tests + 1))
    fi
    total_tests=$((total_tests + 1))
    echo

    if test_process_switching; then
        passed_tests=$((passed_tests + 1))
    fi
    total_tests=$((total_tests + 1))
    echo

    # Summary
    echo "📊 Inventory Validation Summary:"
    echo "   Test suites: $total_tests"
    echo "   Passed: $passed_tests"

    if [ $passed_tests -eq $total_tests ]; then
        echo "   🎉 All inventory tests passed!"
        return 0
    else
        echo "   ❌ Some inventory validations failed"
        return 1
    fi
}

# Export functions for use in main test script
export -f validate_inventory
export -f test_nuclear_power_units
export -f test_steel_mass_conversions
export -f test_refinery_joint_production
export -f test_process_switching
export -f run_inventory_validation