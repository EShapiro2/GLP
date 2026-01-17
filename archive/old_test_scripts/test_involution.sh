#!/bin/bash
cd /Users/udi/GLP/glp_runtime

echo "=== Testing double_involution_error.glp ==="
echo '/Users/udi/GLP/glp_runtime/test/programs/moded_types/invalid/embedded/double_involution_error.glp
:quit' | dart run bin/glp_repl_typed.dart 2>&1

echo ""
echo "=== Testing double_involution.glp (valid) ==="
echo '/Users/udi/GLP/glp_runtime/test/programs/moded_types/valid/embedded/double_involution.glp
:quit' | dart run bin/glp_repl_typed.dart 2>&1
