#!/bin/bash
cd /Users/udi/Grassroots/GLP/glp_runtime

echo "=========================================="
echo "Testing Union Alias Implementation"
echo "=========================================="
echo ""

echo "VALID TESTS (should pass):"
echo "=========================================="

for file in test/programs/moded_types/valid/union_alias*.glp; do
    echo ""
    echo "Testing: $(basename $file)"
    echo "------------------------------------------"
    dart run bin/glpc.dart --type-check "$file" 2>&1 | head -30
done

echo ""
echo ""
echo "INVALID TESTS (should fail with specific errors):"
echo "=========================================="

for file in test/programs/moded_types/invalid/union_alias*.glp; do
    echo ""
    echo "Testing: $(basename $file)"
    echo "------------------------------------------"
    dart run bin/glpc.dart --type-check "$file" 2>&1 | head -30
done

echo ""
echo "=========================================="
echo "Test run complete"
echo "=========================================="
