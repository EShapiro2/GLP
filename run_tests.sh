#!/bin/bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test test/multiagent/ > /tmp/ma-tests.txt 2>&1
echo "Done: $?" >> /tmp/ma-tests.txt
