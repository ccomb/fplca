# SAMPLE.edge - Robustness and Error Handling Test Dataset

This test dataset validates ACV engine robustness against edge cases, malformed data, and error conditions.

## Test Activities

### 1. no-ref-uuid_product-a-uuid.spold
**Purpose**: Missing reference product handling
**Edge cases tested**:
- Activity with no outputGroup="0" exchanges
- Multiple outputs with outputGroup="2", "3" (non-reference)
- Should trigger cut-off strategy to assign reference product

**Expected behavior**:
- Conservative cut-off should assign one non-zero output as reference
- Engine should not crash or fail to process

### 2. loop-a-uuid_product-a-uuid.spold, loop-b-uuid_product-b-uuid.spold, loop-c-uuid_product-c-uuid.spold
**Purpose**: Circular dependency handling (A→B→C→A)
**Edge cases tested**:
- Circular reference chain: A needs B, B needs C, C needs A
- Matrix solver must handle circular dependencies
- Tree traversal should detect and handle loops

**Expected behavior**:
- Matrix assembly should create proper circular references
- PETSc solver should handle the circular system
- Inventory calculations should converge to correct values

### 3. zero-neg-uuid_normal-product-uuid.spold
**Purpose**: Zero and negative amount handling
**Edge cases tested**:
- Zero-amount co-products (should be filtered by cut-off)
- Zero-amount inputs (edge case)
- Negative inputs/outputs (credits, avoided impacts)
- Very small amounts (numerical precision: 1.23456789e-15)

**Expected behavior**:
- Zero co-products removed by cut-off strategy
- Negative values preserved (valid for credits/avoided impacts)
- Small values handled with proper precision
- Matrix assembly threshold (1e-15) applied correctly

### 4. invalid-refs-uuid_valid-product-uuid.spold
**Purpose**: Invalid UUID and reference handling
**Edge cases tested**:
- Missing activityLinkId attribute
- Invalid UUID format in activityLinkId
- Empty activityLinkId ("")
- Valid UUID format but non-existent activity reference
- Whitespace in UUIDs

**Expected behavior**:
- Missing/invalid activityLinkId should be handled gracefully
- Should not create matrix entries for invalid references
- Engine should continue processing valid exchanges
- May generate warnings but should not crash

### 5. xml-edge-uuid_xml-test-product-uuid.spold
**Purpose**: XML parsing edge cases
**Edge cases tested**:
- inputGroup/outputGroup as attributes vs child elements
- Both attribute AND child element (attribute precedence)
- Missing name elements (fallback to flowId)
- Empty name elements
- Missing unitName elements (fallback to default)
- Missing/incomplete compartment structures

**Expected behavior**:
- Parser should prefer attributes over child elements
- Graceful fallbacks for missing elements
- Compartment parsing should handle missing subcompartments
- Default values should be applied appropriately

## Matrix Assembly Validation

### Reference Product Assignment
- Activities without reference products should get one assigned
- Cut-off strategy should be conservative (preserve existing references)
- Zero co-products should be filtered out

### Circular Dependencies
- Matrix should represent circular relationships correctly
- Solver should converge despite circular references
- Inventory results should be mathematically consistent

### Numerical Precision
- Very small values (< 1e-15) should be filtered from matrix
- Zero values should be handled without numerical issues
- Negative values should be preserved where valid

## Testing Commands

```bash
# Test missing reference product handling
./run.sh --data SAMPLE.edge activity no-ref-uuid_product-a-uuid

# Test circular dependency resolution
./run.sh --data SAMPLE.edge inventory loop-a-uuid_product-a-uuid

# Test zero/negative value handling
./run.sh --data SAMPLE.edge activity zero-neg-uuid_normal-product-uuid

# Test invalid reference handling (expect warnings)
./run.sh --data SAMPLE.edge activity invalid-refs-uuid_valid-product-uuid

# Test XML parsing robustness
./run.sh --data SAMPLE.edge activity xml-edge-uuid_xml-test-product-uuid
```

## Expected Warnings/Errors

The invalid references test should generate warnings like:
```
[WARNING] Invalid activityLinkId: not-a-valid-uuid-format
[WARNING] Missing activityLinkId for exchange: missing-link-uuid
[WARNING] Activity not found: 99999999-9999-9999-9999-999999999999
```

The XML parsing test should handle edge cases gracefully without errors.

## Regression Prevention

This dataset prevents:
1. **Parser crashes** on malformed XML or missing elements
2. **Matrix assembly failures** from circular dependencies
3. **Cut-off strategy regressions** - too aggressive reference product removal
4. **Numerical instability** from zero/negative values
5. **UUID validation issues** - invalid references causing crashes
6. **Memory issues** from infinite loops in circular dependencies

## Success Criteria

- ✅ All activities load without crashes
- ✅ Matrix assembly completes for circular dependencies
- ✅ Cut-off strategy assigns reference products appropriately
- ✅ Invalid references generate warnings, not errors
- ✅ Zero values filtered correctly (matrix sparsity)
- ✅ Negative values preserved where appropriate
- ✅ XML parsing handles missing/malformed elements gracefully