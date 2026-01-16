#!/bin/bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo "Testing REPL compilation..."
dart compile exe bin/glp_repl.dart -o /tmp/glp_repl_test 2>&1
if [ $? -eq 0 ]; then
    echo "✓ REPL compiles successfully"
    rm /tmp/glp_repl_test
else
    echo "✗ REPL failed to compile"
fi
