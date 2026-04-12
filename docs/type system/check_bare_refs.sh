#!/bin/bash
cd /Users/udi/Grassroots/GLP

echo "=== Bare Stream in type defs/proc decls (not Stream(X), not FriendStream etc) ==="
grep -rn 'Stream' programs/ --include='*.glp' | grep -v archive | grep -v 'OLD ' | grep -v 'book 2' | grep -v 'Stream(' | grep -v FriendStream | grep -v IntroStream | grep -v OpenStream | grep -v AgentToUserStream | grep -v MediatorToAgentStream | grep -v UserInStream | grep -v NetInStream | grep -v OutputStream | grep -v UserCmdStream | grep -v UserNotifyStream | grep -v GroupStream | grep -v GroupSetupResponseStream | grep -v '%' | grep -E '(::=|procedure)' > /tmp/glp_bare_refs.txt

echo "=== Bare Channel in type defs/proc decls ==="
grep -rn 'Channel' programs/ --include='*.glp' | grep -v archive | grep -v 'OLD ' | grep -v 'book 2' | grep -v 'Channel(' | grep -v FriendChannel | grep -v IntroChannel | grep -v AgentChannel | grep -v ActorChannel | grep -v UserChannel | grep -v GroupChannel | grep -v GroupSetupInviteeChannel | grep -v GroupSetupCreatorChannel | grep -v '%' | grep -E '(::=|procedure)' >> /tmp/glp_bare_refs.txt

echo "=== Bare DiffList ==="
grep -rn 'DiffList' programs/ --include='*.glp' | grep -v archive | grep -v 'OLD ' | grep -v 'book 2' | grep -v 'DiffList(' | grep -v '%' | grep -E '(::=|procedure)' >> /tmp/glp_bare_refs.txt

echo "Done"
