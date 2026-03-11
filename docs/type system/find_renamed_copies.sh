#!/bin/bash
cd /Users/udi/Grassroots/GLP
echo "=== Renamed procedure copies (Section 14 workarounds) ==="
echo ""
echo "--- send_agent ---"
grep -rn 'send_agent' programs/ --include='*.glp' | grep -v archive | grep -v 'OLD '
echo ""
echo "--- send_user ---"
grep -rn 'send_user' programs/ --include='*.glp' | grep -v archive | grep -v 'OLD '
echo ""
echo "--- merge_net_in ---"
grep -rn 'merge_net_in' programs/ --include='*.glp' | grep -v archive | grep -v 'OLD '
echo ""
echo "--- merge_agent ---"
grep -rn 'merge_agent' programs/ --include='*.glp' | grep -v archive | grep -v 'OLD '
echo ""
echo "--- Any procedure with _agent or _user suffix ---"
grep -rn 'procedure.*_agent\|procedure.*_user' programs/ --include='*.glp' | grep -v archive | grep -v 'OLD ' | grep -v send_to_user_tagged
echo "Done"
