# Website handover, 2026-11-20 (b), Website #5, after its first compaction

Supersedes nothing in handover-2026-11-20.md; adds the afternoon's state.

## Live (GLP main)
- Nimrod's five GCE contracts (Nonprofit, Partnership, Foundation, Disputes, Warnings) are live since the 13:11 UTC sweep (main ba7dc9279; Udi's "yes").
- GO's community structure is NOT live: it waits for Udi's separate yes after he reviews /next.

## /next (GLP branch Website-site, head ea0ef60ca)
- Shows GO's structure since the 16:11 UTC sweep, at Udi's "bring this to completion up to contract checking and then upload to /next".
- Local build at ea0ef60ca: 88 contracts.  Community and Backed Community checked, platform compiled, stop at Running.  Democratic Community checked (d25d3de46), no Interface pragmas, stops at Platform.  Community Network parametric, stops at Platform.  Rides, Ads, Lodging, Marketplace, Gigs checked, stop at Platform (their old platforms removed on purpose: "up to contract checking").
- To check after the 17:11 sweep: peoplesweb.org/next/stack/Democratic-Community/verdict.txt.
- Udi was notified with the links at about 16:25 UTC; GO #14 and the coordinator too.

## Rules in force
- When Udi waits for /next, run the sweep at once and tell him (Udi: "when I am waiting for next, you do sweep immediately").  This container cannot reach the server; the sweep is `ssh root@162.35.126.126 systemctl start peoplesweb-sweep.service`, Udi's Terminal or a Remote Control session on his Mac (a card waits for his Allow since 11:41 UTC).  Otherwise the hourly :11 sweep carries the change.
- Every message to Udi passes `python3 -I tools/myenglish/check.py --udi`.
- A visible change to the live site needs Udi's yes.  Only Code merges to main; only Website pushes Website-site.
- Copied verdicts can be stale: show a contract checked only when its cascade verdict at that commit says so.
- Check /next through the small full-contract.txt files, not the detail JSON (truncated by the fetcher).

## Threads
- Code #36: cmsg_0189yZceJmej5oE7scfYctePPPVYvCNSFDGXJypvR76sJe
- Calculus #10: cmsg_0189yZceJmej5oE7scfYcteP6eCUK4ZCKt4Kb7M12UyUGu
- GO #14: cmsg_0189yZceJmej5oE7scfYctePBB9fXVNpv6jTqz95ZumkZt

## Next
1. After 17:11 UTC, confirm Democratic Community shows checked on /next.
2. Await Udi's review; on his yes, ask Code to bring the structure to main for the live site.
