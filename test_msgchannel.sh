#!/bin/bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo "Testing with MsgChannel and msg_merge:"
echo "======================================"
dart run bin/glpc.dart --type-check glp/social_agent_typed.glp 2>&1 | head -80
