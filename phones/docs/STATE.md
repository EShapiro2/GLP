# Phones --- state, 2026-11-10 13:39 UTC (Phones #2 Cowork)

Taken over from `tmp/phones/STATE.md` (Code #25B Phones, 13:57 UTC) and Code #25's handover (Code_inbox.md, 13:36 and 13:38 UTC).

## Devices

- 11 Pro: UDID `00008030-000275600A12802E`, iOS 17.4.1, phone key `33ac34ed...`; the reporter build of GLP 1ecdd1e8 since 2026-11-09 13:28 UTC.
- 16 Pro: UDID `00008140-001C38C83A12801C`, iOS 26.6.1, phone key `6c2c1695...`; the reporter build of GLP 1ecdd1e8 since 2026-11-09 13:29 UTC.
- Both phones are to be rebuilt, the app and its data deleted first, so each will have a new key; the keys above stay as the names of their log runs.  The de558755 build of 2026-11-09 23:02 UTC was never installed.  Main 01253c9c changes the code format to 4 and the saved-state format to 7, so the phones are built from the commit the servers are updated to in the same sitting, 01253c9c or later (Code #27, Phones_inbox.md 2026-11-10 11:03 UTC).
- The Mac as an agent: a team-signed debug build with the Keychain entitlement starts but shows no window (Code #25, 13:38 UTC); the entitlement edit is not in the tree.

## Servers

The rendezvous server, 162.35.126.126:9516, and the directory, :9518, run GLP main e45d656f since 2026-11-10 10:12 UTC, protocol 9 and code format 3, the directory's key 6eb240e8 kept (Code #27, Phones_inbox.md 10:15 UTC).

## Signing

The free team signs one phone per build; the paid account waits on Apple Developer Support, Apple's enrolment refusing an Israeli ID.

## What has run

- 2026-11-08 18:12--18:25 UTC, `p1-two-phones-lan` by hand, on the reporter build (GLP fe9e29fc in Code's build clone, profile mode, one build for both phones, the profile listing both until 2026-11-15): all seven steps met; but Bluetooth was on, and the session ran over `pan`, so what ran is `p1-two-phones-ble`'s steps 1 to 7, and the LAN play is still to run.  Logs: 16 Pro (phone `6c2c1695...`) runs 1794161740557 and 1794162188976; 11 Pro (phone `33ac34ed...`) runs 1794161825448 and 1794162267924.  Six faults (Code #26 to Phones, 18:20 and 18:25 UTC): 1, 2, 5, 6 sent to Code and 3 to GSA (18:33 UTC); 4 is the play's network.
- The two phones' clocks differ by about 76 s, the 11 Pro behind: events of two phones are ordered by the service's arrival stamp.

- 2026-11-08 18:41 UTC, the scripted befriending on two simulators on one Mac (Phones Mac, 18:47 UTC): not met.  1. The layer drops an mDNS presence resolved under Closed LAN trust and does not contact it when the trust turns Open: Code's, Cloud #8 replaying the presences (Code to GSA, 18:48 UTC).  2. Two simulators on one Mac see each other at 127.0.0.1, and loopback is not a local network (GNA, Implementation Notes, "The local network"), so two simulators on one Mac do not run a LAN play.
- 2026-11-08 19:05--19:11 UTC, `p1-two-phones-ble` by hand, Wi-Fi off (Phones Mac, 19:12 UTC): steps 1 to 6 met; step 7 failed: after a restart the agent makes no declaration to the layer, so the friends do not reconnect; to GSA and GLP, Blocking: yes (18:39 UTC).  The 11 Pro has no cellular path, so its log reaches the service only when it has Wi-Fi.

- 2026-11-08 20:06--20:10 UTC, `p1-two-phones-ble` step 8 (Phones Mac, 20:11 UTC): met; the 16 Pro's layer kept the Bluetooth session through a minute in the background.  Not tested: a message waiting while in the background.

- 2026-11-08 20:51--20:59 UTC, invite by link over the Internet, GRV (Phones Mac, 21:01 UTC): the link made, registered and redeemed, GRV answering both with `punch(Key, Address)`; no session formed between a phone on cellular and one behind the venue's NAT.
- 2026-11-09 11:29--11:38 UTC, `p1-two-phones-lan`: not run to its end: the venue's Wi-Fi put the phones on two subnets, and on a Personal Hotspot neither resolved the other.  The replay of presences resolved under Closed trust works.  Two network faults with Code (11:36, 11:38 UTC); a lost path recovered with no restart on Cloud-netroam (13:29 UTC).
- 2026-11-09 11:40--11:45 UTC, `p1-two-phones-ble`: steps 1 to 7 met; after a restart each agent declared its friend and both trusts and reconnected over Bluetooth within 6 s.  With step 8 of 2026-11-08, the Bluetooth play is met whole.
- 2026-11-09 12:39--12:49 UTC, GDS: List, the verification mail, the supply of links, a lookup on the directory's page, the link opened in the other phone and redeemed at GRV, and Unlist: met.  A session over the Internet is not yet tested: no punch has made one.
- 2026-11-09 14:25--14:32 UTC: Dan filed 18 issues labelled `phones`, #10 to #27, faults of the networking code against GNA found by an audit, and seventeen pull requests, #28 to #44, each closing one of #10 to #26; #27 has none yet; sent to Code.

## Next

1. The log service and the reporter (`docs/debugging.md`), approved by Udi 15:19 UTC, tasked to Code 15:20 UTC, parcelled by Code to the clouds Debuglog and Reporter.
2. `p1-two-phones-lan` on the two phones, judged from their logs; then `p1-two-phones-ble`; then `p1-three-phones`, which waits on an introduction on home (GSA_inbox.md 15:48 UTC).
