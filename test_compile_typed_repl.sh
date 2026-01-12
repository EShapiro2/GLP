#!/bin/bash
cd /Users/udi/GLP/glp_runtime
echo "Testing typed REPL compilation..."
dart compile exe bin/glp_repl_typed.dart -o /tmp/glp_repl_typed_test 2>&1
if [ $? -eq 0 ]; then
    echo "✓ Typed REPL compiles successfully"
    rm /tmp/glp_repl_typed_test
else
    echo "✗ Typed REPL failed to compile"
fi
