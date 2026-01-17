#!/bin/bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo "Testing with MsgStream - Bug 3 should be fixed:"
echo "================================================"
dart run bin/glpc.dart --type-check glp/social_agent_typed.glp 2>&1 | grep -E "(Variable Id\?|TYPE ERROR|TYPE WARNING|Compilation)" | head -30
