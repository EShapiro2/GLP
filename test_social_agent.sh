#!/bin/bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo "Testing social_agent_typed.glp to reproduce bugs from 2026-01-17 report:"
echo "=========================================="
dart run bin/glpc.dart --type-check glp/social_agent_typed.glp 2>&1
