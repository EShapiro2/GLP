#!/bin/bash
# Test Bug 4 fix: Anonymous variables should be treated as independent
# Run from GLP directory

cd /Users/udi/Grassroots/GLP/glp_runtime

echo "=== Testing Bug 4 fix: Anonymous Variables ==="
echo ""
echo "Testing social_agent_typed.glp (should show reduction in errors):"
dart run bin/glpc.dart --type-check glp/social_agent_typed.glp 2>&1

echo ""
echo "=== Done ==="
