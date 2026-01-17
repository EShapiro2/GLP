#!/bin/bash
cd /Users/udi/GLP/glp_runtime
echo "=== Testing simple_io.glp ==="
echo "/Users/udi/GLP/glp_runtime/test/programs/moded_types/valid/simple_io.glp" | dart run bin/glp_repl_typed.dart 2>&1

echo ""
echo "=== Testing counter_show.glp ==="
echo "/Users/udi/GLP/glp_runtime/test/programs/moded_types/valid/embedded/counter_show.glp" | dart run bin/glp_repl_typed.dart 2>&1

echo ""
echo "=== Testing moded_types/valid/merge.glp ==="
echo "/Users/udi/GLP/glp_runtime/test/programs/moded_types/valid/merge.glp" | dart run bin/glp_repl_typed.dart 2>&1

echo ""
echo "=== Testing moded_types/valid/append.glp ==="
echo "/Users/udi/GLP/glp_runtime/test/programs/moded_types/valid/append.glp" | dart run bin/glp_repl_typed.dart 2>&1
