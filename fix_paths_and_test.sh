#!/bin/bash
# Fix hardcoded paths and establish test baseline
# Run from /Users/udi/Grassroots/GLP directory
# Output: /private/tmp/test_baseline.txt

set -e

echo "=========================================="
echo "  GLP Path Fix and Test Baseline Script  "
echo "=========================================="
echo ""

# 1. Fix hardcoded paths in wrapper scripts
echo "Step 1: Fixing hardcoded paths..."

# Fix run_main_repl_tests.sh
sed -i.bak 's|/Users/udi/GLP|/Users/udi/Grassroots/GLP|g' /Users/udi/Grassroots/GLP/run_main_repl_tests.sh
echo "  - Fixed run_main_repl_tests.sh"

# Fix run_typed_repl_tests.sh
sed -i.bak 's|/Users/udi/GLP|/Users/udi/Grassroots/GLP|g' /Users/udi/Grassroots/GLP/run_typed_repl_tests.sh
echo "  - Fixed run_typed_repl_tests.sh"

# Fix STATUS.md paths
sed -i.bak 's|/Users/udi/GLP|/Users/udi/Grassroots/GLP|g' "/Users/udi/Grassroots/GLP/docs/type system/STATUS.md"
echo "  - Fixed docs/type system/STATUS.md"

# Fix test/README.md paths
sed -i.bak 's|/Users/udi/GLP|/Users/udi/Grassroots/GLP|g' /Users/udi/Grassroots/GLP/test/README.md
echo "  - Fixed test/README.md"

echo ""
echo "Step 2: Creating test_output directory..."
mkdir -p /Users/udi/Grassroots/GLP/test_output
echo "  - Created /Users/udi/Grassroots/GLP/test_output"

echo ""
echo "Step 3: Running main REPL test suite (222 tests)..."
cd /Users/udi/Grassroots/GLP
./test/full_run_repl_tests.sh > /Users/udi/Grassroots/GLP/test_output/main_repl_baseline.txt 2>&1
MAIN_EXIT=$?

echo ""
echo "Step 4: Running typed REPL test suite (222 tests)..."
./test/run_typechecker_repl_tests.sh > /Users/udi/Grassroots/GLP/test_output/typed_repl_baseline.txt 2>&1
TYPED_EXIT=$?

echo ""
echo "=========================================="
echo "  Results Summary"
echo "=========================================="

# Parse main REPL results
MAIN_TOTAL=$(grep "Total:" /Users/udi/Grassroots/GLP/test_output/main_repl_baseline.txt | tail -1 | sed 's/.*Total: \([0-9]*\).*/\1/')
MAIN_PASS=$(grep "Passed:" /Users/udi/Grassroots/GLP/test_output/main_repl_baseline.txt | tail -1 | sed 's/.*Passed: \([0-9]*\).*/\1/')
MAIN_FAIL=$(grep "Failed:" /Users/udi/Grassroots/GLP/test_output/main_repl_baseline.txt | tail -1 | sed 's/.*Failed: \([0-9]*\).*/\1/')

echo ""
echo "Main REPL Tests (untyped):"
echo "  Total: $MAIN_TOTAL | Passed: $MAIN_PASS | Failed: $MAIN_FAIL"
echo "  Output: /Users/udi/Grassroots/GLP/test_output/main_repl_baseline.txt"

# Parse typed REPL results
TYPED_TOTAL=$(grep "Total:" /Users/udi/Grassroots/GLP/test_output/typed_repl_baseline.txt | tail -1 | sed 's/.*Total: \([0-9]*\).*/\1/')
TYPED_PASS=$(grep "Passed:" /Users/udi/Grassroots/GLP/test_output/typed_repl_baseline.txt | tail -1 | sed 's/.*Passed: \([0-9]*\).*/\1/')
TYPED_FAIL=$(grep "Failed:" /Users/udi/Grassroots/GLP/test_output/typed_repl_baseline.txt | tail -1 | sed 's/.*Failed: \([0-9]*\).*/\1/')

echo ""
echo "Typed REPL Tests (with type checking):"
echo "  Total: $TYPED_TOTAL | Passed: $TYPED_PASS | Failed: $TYPED_FAIL"
echo "  Output: /Users/udi/Grassroots/GLP/test_output/typed_repl_baseline.txt"

# Create summary report
cat > /Users/udi/Grassroots/GLP/test_output/test_baseline_summary.txt <<EOF
GLP Test Baseline Report
Generated: $(date)
========================================

Main REPL Tests (untyped - programs/tests/repl):
  Total:  $MAIN_TOTAL
  Passed: $MAIN_PASS
  Failed: $MAIN_FAIL
  Exit:   $MAIN_EXIT
  Output: /Users/udi/Grassroots/GLP/test_output/main_repl_baseline.txt

Typed REPL Tests (with type checker - programs/typed_book):
  Total:  $TYPED_TOTAL
  Passed: $TYPED_PASS
  Failed: $TYPED_FAIL
  Exit:   $TYPED_EXIT
  Output: /Users/udi/Grassroots/GLP/test_output/typed_repl_baseline.txt

Path Fixes Applied:
  - run_main_repl_tests.sh: /Users/udi/GLP → /Users/udi/Grassroots/GLP
  - run_typed_repl_tests.sh: /Users/udi/GLP → /Users/udi/Grassroots/GLP
  - docs/type system/STATUS.md: paths updated
  - test/README.md: paths updated

Backup files created (*.bak) in case rollback needed.
========================================
EOF

echo ""
echo "Summary saved to: /Users/udi/Grassroots/GLP/test_output/test_baseline_summary.txt"
echo ""
echo "DONE! Please review the output files."
exit 0
