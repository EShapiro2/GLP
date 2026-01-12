#!/bin/bash
cd /Users/udi/GLP/glp_runtime

echo "=== Testing simple_io.glp ==="
echo '/Users/udi/GLP/glp_runtime/test/programs/moded_types/valid/simple_io.glp
:quit' | dart run bin/glp_repl_typed.dart 2>&1

echo ""
echo "=== Testing counter_show.glp ==="
echo '/Users/udi/GLP/glp_runtime/test/programs/moded_types/valid/embedded/counter_show.glp
:quit' | dart run bin/glp_repl_typed.dart 2>&1
