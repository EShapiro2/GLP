#!/bin/bash
# Test and run script for irmaGLP fix

echo "=== Running glp_runtime tests ==="
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test

echo ""
echo "=== Running Flutter multiagent app ==="
cd /Users/udi/Grassroots/GLP/glp_multiagent && flutter run -d macos
