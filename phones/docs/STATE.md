# Phones --- state, 2026-11-08 18:35 UTC (Phones #1 Cowork)

Taken over from `tmp/phones/STATE.md` (Code #25B Phones, 13:57 UTC) and Code #25's handover (Code_inbox.md, 13:36 and 13:38 UTC).

## Devices

- 11 Pro: UDID `00008030-000275600A12802E`, iOS 17.4.1; GrassApp at GLP main 8031c235 installed 13:08 UTC by `ideviceinstaller`, developer trusted, opened; signed for it by the free team G374822FDT.
- 16 Pro: UDID `00008140-001C38C83A12801C`, iOS 26.6.1; GrassApp at GLP main 8031c235 since 14:45 UTC (Code #26).
- The Mac as an agent: a team-signed debug build with the Keychain entitlement starts but shows no window (Code #25, 13:38 UTC); the entitlement edit is not in the tree.

## Signing

The free team signs one phone per build; the paid account waits on Apple Developer Support, Apple's enrolment refusing an Israeli ID.

## What has run

- 2026-11-08 18:12--18:25 UTC, `p1-two-phones-lan` by hand, on the reporter build (GLP fe9e29fc in Code's build clone, profile mode, one build for both phones, the profile listing both until 2026-11-15): all seven steps met; but Bluetooth was on, and the session ran over `pan`, so what ran is `p1-two-phones-ble`'s steps 1 to 7, and the LAN play is still to run.  Logs: 16 Pro (phone `6c2c1695...`) runs 1794161740557 and 1794162188976; 11 Pro (phone `33ac34ed...`) runs 1794161825448 and 1794162267924.  Six faults (Code #26 to Phones, 18:20 and 18:25 UTC): 1, 2, 5, 6 sent to Code and 3 to GSA (18:33 UTC); 4 is the play's network.
- The two phones' clocks differ by about 76 s, the 11 Pro behind: events of two phones are ordered by the service's arrival stamp.

## Next

1. The log service and the reporter (`docs/debugging.md`), approved by Udi 15:19 UTC, tasked to Code 15:20 UTC, parcelled by Code to the clouds Debuglog and Reporter.
2. `p1-two-phones-lan` on the two phones, judged from their logs; then `p1-two-phones-ble`; then `p1-three-phones`, which waits on an introduction on home (GSA_inbox.md 15:48 UTC).
