#!/bin/bash
# Diagnose next 5 failing tests
cd /Users/udi/Grassroots/GLP/glp_runtime
REPL="bin/glp_repl.dart"

echo "=========================================="
echo "DIAGNOSIS BATCH 2"
echo "=========================================="
echo ""

# Next 5 failing tests
echo "TEST 6: merge_ordered.glp"
cat > /tmp/test6.glp << 'EOF'
test/../programs/typed_book/recursive/list_processing/merge_ordered.glp
:quit
EOF
cat /tmp/test6.glp | dart run "$REPL" 2>&1 | tail -30
echo ""
echo "===================="
echo ""

echo "TEST 7: nth.glp"
cat > /tmp/test7.glp << 'EOF'
test/../programs/typed_book/recursive/list_processing/nth.glp
:quit
EOF
cat /tmp/test7.glp | dart run "$REPL" 2>&1 | tail -30
echo ""
echo "===================="
echo ""

echo "TEST 8: polygon_area.glp"
cat > /tmp/test8.glp << 'EOF'
test/../programs/typed_book/recursive/list_processing/polygon_area.glp
:quit
EOF
cat /tmp/test8.glp | dart run "$REPL" 2>&1 | tail -30
echo ""
echo "===================="
echo ""

echo "TEST 9: ancestor.glp"
cat > /tmp/test9.glp << 'EOF'
test/../programs/typed_book/recursive/structure_processing/ancestor.glp
:quit
EOF
cat /tmp/test9.glp | dart run "$REPL" 2>&1 | tail -30
echo ""
echo "===================="
echo ""

echo "TEST 10: distribute_nonground.glp"
cat > /tmp/test10.glp << 'EOF'
test/../programs/typed_book/recursive/structure_processing/distribute_nonground.glp
:quit
EOF
cat /tmp/test10.glp | dart run "$REPL" 2>&1 | tail -30
echo ""
