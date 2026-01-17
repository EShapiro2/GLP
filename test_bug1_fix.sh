#!/bin/bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo "Testing Bug 1 fix - atoms should now be recognized as strings:"
echo "================================================================"
dart run bin/glpc.dart --type-check glp/social_agent_typed.glp 2>&1 | grep -E "(user|Constant|TYPE ERROR|TYPE WARNING|Compilation)" | head -30
