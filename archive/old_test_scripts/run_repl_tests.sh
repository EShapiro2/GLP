#!/bin/bash
cd /Users/udi/GLP
bash test/run_typechecker_repl_tests.sh 2>&1 | tee /tmp/repl_test_output.txt
echo ""
echo "Output saved to /tmp/repl_test_output.txt"
