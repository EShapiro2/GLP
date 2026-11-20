# Website handover, 2026-11-20 (b), Website #5

Written after Website #5's first compaction, updated 2026-11-20 23:05 UTC.  It replaces handover-2026-11-20.md and handover-2026-11-17.md, which git holds.

## What a thread reads (Coordination, 2026-11-20 22:59 UTC, on Udi's yes at 22:52)
At its start and after a compaction: the three rules files of GLP `organisation/`, then its project's entry in `organisation/projects.md`, the record it names, and its latest handover.  Website's record is `docs/decisions.md` here, named in projects.md (GLP 27b0c4447, Coordination #17); it replaces `CLAUDE.md`, removed on Website-site.  Read it, `README.md` here and this handover.

## Live (GLP main)
- Nimrod's five GCE contracts are live since the 13:11 UTC sweep (Udi's yes).
- GO's community structure is NOT live: it waits for Udi's yes after his review of /next.
- Live still shows stale platforms for the contracts Code #36 reprinted on its branch; main is Code's.
- Not approved for live: LWG's listing; step two of Matching.

## /next (GLP branch Website-site)
- Built within a minute of each push to Website-site, since 2026-11-20 21:11 UTC (Udi's yes at 20:26 UTC to the Optimiser's proposal): `deploy/next.sh`, `peoplesweb-next.{timer,service}`, installed by `sweep.sh`, one flock with the sweep (main b2dc3c8b3, 0d18b4347).  Confirmed: push 22:03:49, served by 22:10.  `systemctl start peoplesweb-next.service` builds it at once.
- GO's structure: Community, Democratic Community, Backed Community, Community Network, and Rides, Ads, Lodging, Marketplace, Gigs in community coins.
- Records are taken from Code #36's branch claude/project-thread-llohtv as it sends commits (last 2d0f7a1ab, merged as bb17d0fe2).  A platform is shown only when it carries the current contract text.  Still off: Rides' and Gigs' platforms, waiting on GO's mend of a pragma of Community Network.

## Rules in force
- When Udi waits for /next, push and tell him once it is built.
- Every message to Udi passes `python3 -I tools/myenglish/check.py --udi`; post to Udi only when his response is needed.
- A visible change to the live site needs Udi's yes; only Code merges to main; only Website pushes Website-site.
- Show a contract checked, or a platform, only from a record printed from its current text.
- To read /next, fetch small files (stack/<Name>/verdict.txt, full-contract.txt); the fetcher truncates data.js.

## Threads
- Code #36: cmsg_0189yZceJmej5oE7scfYctePPPVYvCNSFDGXJypvR76sJe
- Calculus #10: cmsg_0189yZceJmej5oE7scfYcteP6eCUK4ZCKt4Kb7M12UyUGu
- GO #14: cmsg_0189yZceJmej5oE7scfYctePBB9fXVNpv6jTqz95ZumkZt

## Next
1. Merge each record commit Code sends into Website-site and push; Rides and Gigs when GO mends Community Network's pragma.
2. On Udi's yes to GO's structure, ask Code to merge it into main, quoting his words.
3. Step two of Matching and LWG: merge into Website-site when Code's commits come; LWG goes live only on Udi's yes.
