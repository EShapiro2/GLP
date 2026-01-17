#!/bin/bash
cd /Users/udi/GLP/glp_runtime

echo "=== Testing any_constant_at_output.glp ==="
echo '/Users/udi/GLP/glp_runtime/test/programs/moded_types/invalid/universal/any_constant_at_output.glp
:quit' | dart run bin/glp_repl_typed.dart 2>&1

echo ""
echo "=== Testing any_constant_at_input.glp ==="
echo '/Users/udi/GLP/glp_runtime/test/programs/moded_types/invalid/universal/any_constant_at_input.glp
:quit' | dart run bin/glp_repl_typed.dart 2>&1
