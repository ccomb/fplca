# SAMPLE.switching - Process Switching and Reference Product Test Dataset

This test dataset validates ACV engine handling of **same activity UUID with different reference products** - the critical scenario for process flexibility, market-driven allocation, and database merging.

## Test Scenarios

### 1. Chemical Plant Switching (UUID: 22222222-3333-4444-5555-666666666661)

**Purpose**: Tests same facility producing different chemicals with reciprocal product amounts

**Files**:
- `22222222-3333-4444-5555-666666666661_chemical-a-uuid.spold` - Mode A (Chemical A primary)
- `22222222-3333-4444-5555-666666666661_chemical-b-uuid.spold` - Mode B (Chemical B primary)

**Products tested**:
- **Mode A**: Chemical A = 1.0 kg (reference), Chemical B = 0.0 kg (zero co-product)
- **Mode B**: Chemical A = 0.0 kg (zero co-product), Chemical B = 1.0 kg (reference)

**Key validation**:
- Same activity UUID, different ProcessId filenames
- Reciprocal reference product switching (1↔0, 0↔1)
- Different input requirements per mode (Catalyst A vs Catalyst B)
- Different environmental profiles per mode
- Tests cut-off strategy with zero co-products

### 2. Power Plant Fuel Switching (UUID: 33333333-4444-5555-6666-777777777772)

**Purpose**: Tests same power plant operating on different fuels with different primary outputs

**Files**:
- `33333333-4444-5555-6666-777777777772_coal-power-uuid.spold` - Coal mode (electricity primary)
- `33333333-4444-5555-6666-777777777772_gas-power-uuid.spold` - Gas mode (heat primary)

**Products tested**:
- **Coal mode**: Electricity = 1.0 MJ (reference), Heat = 0.5 MJ (co-product)
- **Gas mode**: Heat = 1.0 MJ (reference), Electricity = 0.8 MJ (co-product)

**Key validation**:
- Same activity UUID, different fuel inputs
- Different reference product selection (electricity vs heat)
- Different co-product ratios and environmental impacts
- Tests realistic CHP (combined heat and power) scenarios

## Matrix Assembly Challenges

### ProcessId Disambiguation
The ProcessId system must handle:
- Same activity UUID appearing in multiple files
- Different reference products requiring different matrix columns
- Filename-based disambiguation: `activity_uuid_reference_product_uuid.spold`

### Reference Product Selection
- Each file defines its own reference product (outputGroup="0")
- Matrix assembly must respect file-specific reference products
- Cut-off strategy should filter zero co-products appropriately

### Supply Chain Integration
- Other processes can link to specific modes via ProcessId
- Inventory calculation should depend on which mode is selected
- Database merging scenarios: same process, different contexts

## Expected Matrix Behavior

### Technosphere Matrix Structure
```
Activities:  [ChemPlantA, ChemPlantB, PowerCoal, PowerGas, ...]
Products:    [ChemicalA,  ChemicalB,  Electricity, Heat, ...]

ChemPlantA:  [ 1.0,       0.0,       0.0,         0.0, ...]  // Mode A
ChemPlantB:  [ 0.0,       1.0,       0.0,         0.0, ...]  // Mode B
PowerCoal:   [ 0.0,       0.0,       1.0,         0.5, ...]  // Coal CHP
PowerGas:    [ 0.0,       0.0,       0.8,         1.0, ...]  // Gas CHP
```

### Inventory Validation Targets
- **Chemical A mode**: CO2 = 0.8 kg, Chemical A VOCs = 5.0 g
- **Chemical B mode**: CO2 = 1.2 kg, Chemical B VOCs = 8.0 g
- **Coal power mode**: CO2 = 0.35 kg, SOx = 2.0 g
- **Gas power mode**: CO2 = 0.20 kg, NOx = 1.5 g

## Testing Commands

```bash
# Test chemical plant modes
./run.sh --data SAMPLE.switching activity 22222222-3333-4444-5555-666666666661_chemical-a-uuid
./run.sh --data SAMPLE.switching activity 22222222-3333-4444-5555-666666666661_chemical-b-uuid

# Test power plant modes
./run.sh --data SAMPLE.switching activity 33333333-4444-5555-6666-777777777772_coal-power-uuid
./run.sh --data SAMPLE.switching activity 33333333-4444-5555-6666-777777777772_gas-power-uuid

# Test inventory correctness
./run.sh --data SAMPLE.switching inventory 22222222-3333-4444-5555-666666666661 --format csv
./run.sh --data SAMPLE.switching inventory 33333333-4444-5555-6666-777777777772 --format csv
```

## Regression Prevention

This dataset prevents:
1. **ProcessId collision errors** - same UUID, different files
2. **Reference product confusion** - matrix using wrong primary output
3. **Database merging bugs** - same process, different contexts
4. **Allocation inconsistencies** - different modes having inconsistent allocation
5. **Cut-off strategy errors** - zero co-products mishandled
6. **Inventory calculation bugs** - mode-specific emissions not properly attributed

## Success Criteria

- ✅ All 4 activities load without ProcessId conflicts
- ✅ Reference products correctly identified per file
- ✅ Zero co-products filtered by cut-off strategy
- ✅ Matrix assembly creates separate columns for each mode
- ✅ Inventory calculations reflect mode-specific environmental impacts
- ✅ No UUID collision errors during database loading
- ✅ ProcessId system correctly disambiguates same UUID, different reference products