# SAMPLE.multiproduct - Multi-Output Allocation Test Dataset

This test dataset validates ACV engine handling of multi-output processes and allocation strategies.

## Test Activities

### 1. 11111111-2222-3333-4444-555555555555_gasoline-uuid.spold
**Purpose**: Joint production with multiple valuable co-products
**Products tested**:
- Gasoline: 40.0 l (reference product, outputGroup="0")
- Diesel: 35.0 l (co-product, outputGroup="2")
- Heating oil: 20.0 l (co-product, outputGroup="3")
- Heavy fuel oil: 5.0 l (co-product, outputGroup="4")

**Key validation**:
- Tests realistic refinery process (crude oil → multiple fuel products)
- All products have significant non-zero amounts
- Different outputGroup values (0, 2, 3, 4)
- Environmental burdens (CO2, SO2, wastewater) allocated across all products

**Expected behavior**:
- Gasoline should be identified as reference product
- Cut-off strategy should preserve all non-zero co-products
- Matrix should represent all four output products correctly

### 2. aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee_lumber-uuid.spold
**Purpose**: Allocation sensitivity testing with different mass/value ratios
**Products tested**:
- Lumber: 0.6 m³ (reference product, high value/low mass)
- Sawdust: 200.0 kg (co-product, low value/high mass)
- Bark: 100.0 kg (co-product, often treated as waste)
- Wood chips: 80.0 kg (co-product, medium value)

**Key validation**:
- Tests different unit mixing (m³ vs kg products)
- Demonstrates allocation challenge (mass vs economic allocation would differ significantly)
- Realistic sawmill mass balance
- Different relative values of co-products

**Expected behavior**:
- Lumber should be reference product despite lower mass
- All co-products preserved (no zero amounts)
- Unit conversion should handle m³/kg mixing correctly
- Environmental burdens distributed across all products

### 3. ffffffff-aaaa-bbbb-cccc-dddddddddddd_main-chemical-uuid.spold
**Purpose**: Cut-off strategy testing with zero co-products
**Products tested**:
- Main chemical: 1.0 kg (reference product)
- By-product 1: 0.0 kg (zero co-product - should be filtered)
- By-product 2: 0.0 kg (zero co-product - should be filtered)
- Trace by-product: 1.0e-12 kg (edge case for filtering threshold)
- Waste product: -0.1 kg (negative amount - disposal credit)

**Key validation**:
- Tests cut-off strategy implementation
- Zero amount filtering (removeZeroAmountCoproducts)
- Very small amount handling (numerical precision)
- Negative co-products (disposal credits)
- Edge case: should preserve main product, filter zeros

**Expected behavior**:
- Main chemical should remain as reference product
- Zero co-products should be filtered out by cut-off strategy
- Trace co-product might be filtered (depends on 1e-15 threshold)
- Negative waste product should be preserved (valid for disposal credits)
- No crashes from zero/negative amounts

## Cut-off Strategy Validation

### Conservative Cut-off Behavior
The current implementation uses conservative cut-off:
1. **Filter zero co-products**: `amount == 0.0` products removed
2. **Preserve existing reference products**: Don't reassign if reference already exists
3. **Assign reference only if none exist**: Last resort assignment

### Reference Product Selection
- Activities should have exactly one reference product (`outputGroup="0"`)
- If multiple reference products exist, keep them (conservative)
- If no reference product exists, assign one from non-zero outputs

### Matrix Assembly Impact
- Each output creates potential supply in technosphere matrix
- Zero co-products create zero matrix entries (filtered for sparsity)
- Reference products affect normalization denominator

## Testing Commands

```bash
# Test joint production (multiple valuable co-products)
./run.sh --data SAMPLE.multiproduct activity 11111111-2222-3333-4444-555555555555_gasoline-uuid

# Test allocation sensitivity (mixed units, different value ratios)
./run.sh --data SAMPLE.multiproduct activity aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee_lumber-uuid

# Test cut-off strategy (zero co-products filtering)
./run.sh --data SAMPLE.multiproduct activity ffffffff-aaaa-bbbb-cccc-dddddddddddd_main-chemical-uuid

# Test full supply chain with multi-product processes
./run.sh --data SAMPLE.multiproduct inventory 11111111-2222-3333-4444-555555555555_gasoline-uuid
```

## Expected Matrix Behavior

### Technosphere Matrix Structure
- Each activity contributes ONE column (normalized to reference product)
- Multiple outputs create multiple potential rows (supply of different products)
- Zero co-products should NOT appear in matrix (filtered)

### Reference Product Normalization
With the **fixed normalization** (no double normalization):
- All inputs/outputs normalized per 1 unit of reference product
- Environmental burdens scaled by reference product amount
- Matrix assembly should use single normalization step

## Quality Validation

### Consistency Checks
- ✅ Mass balance: inputs ≈ outputs (within reasonable margins)
- ✅ Reference product identification: exactly one per activity
- ✅ Zero co-product filtering: no zero amounts in final exchanges
- ✅ Unit consistency: compatible units within same flow

### Allocation Logic Verification
- All co-products should receive environmental burden allocation
- Reference product selection should be deterministic
- Cut-off should be conservative (preserve valid products)

## Regression Prevention

This dataset prevents:
1. **Cut-off strategy regressions** - too aggressive filtering
2. **Reference product assignment errors** - multiple or missing reference products
3. **Zero amount handling bugs** - crashes from zero co-products
4. **Unit conversion issues** - mixed units in multi-product processes
5. **Matrix sparsity problems** - zero entries affecting solver performance
6. **Negative amount errors** - disposal credits should be preserved

## Success Criteria

- ✅ All multi-product activities load without errors
- ✅ Cut-off strategy filters zero co-products appropriately
- ✅ Reference products correctly identified (one per activity)
- ✅ Mixed units handled correctly (l, kg, m³, MJ)
- ✅ Negative amounts preserved for disposal credits
- ✅ Matrix assembly succeeds with realistic sparsity
- ✅ Environmental burdens allocated across all products