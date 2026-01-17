#!/bin/bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo "Checking if Msg union alias was properly expanded:"
echo "==================================================="
cat glp/social_agent_typed.glp | grep -A2 "^Msg ::"
echo ""
echo "Running type checker with verbose output:"
dart run bin/glpc.dart --type-check glp/social_agent_typed.glp 2>&1 | head -100
