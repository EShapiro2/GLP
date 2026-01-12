#!/bin/bash
cd /Users/udi/GLP/glp_runtime
echo "=== Compiling typed REPL ==="
dart compile exe bin/glp_repl_typed.dart -o /tmp/glp_repl_typed 2>&1
echo ""
echo "=== Running type checker tests ==="
cd /Users/udi/GLP
chmod +x test/run_typechecker_repl_tests.sh
bash test/run_typechecker_repl_tests.sh 2>&1
