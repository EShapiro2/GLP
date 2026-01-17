#!/bin/bash
# Get error messages for merge_ordered and polygon_area
cd /Users/udi/Grassroots/GLP/glp_runtime

echo "=========================================="
echo "BUG #6: merge_ordered.glp"
echo "=========================================="
echo "../programs/typed_book/recursive/list_processing/merge_ordered.glp
:quit" | dart run bin/glp_repl.dart 2>&1
echo ""
echo ""

echo "=========================================="
echo "BUG #8: polygon_area.glp"
echo "=========================================="
echo "../programs/typed_book/recursive/list_processing/polygon_area.glp
:quit" | dart run bin/glp_repl.dart 2>&1
