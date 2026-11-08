# Phones --- state, 2026-11-08 15:50 UTC (Phones #1 Cowork)

Taken over from `tmp/phones/STATE.md` (Code #25B Phones, 13:57 UTC) and Code #25's handover (Code_inbox.md, 13:36 and 13:38 UTC).

## Devices

- 11 Pro: UDID `00008030-000275600A12802E`, iOS 17.4.1; GrassApp at GLP main 8031c235 installed 13:08 UTC by `ideviceinstaller`, developer trusted, opened; signed for it by the free team G374822FDT.
- 16 Pro: UDID `00008140-001C38C83A12801C`, iOS 26.6.1; GrassApp at GLP main 8031c235 since 14:45 UTC (Code #26).
- The Mac as an agent: a team-signed debug build with the Keychain entitlement starts but shows no window (Code #25, 13:38 UTC); the entitlement edit is not in the tree.

## Signing

The free team signs one phone per build; the paid account waits on Apple Developer Support, Apple's enrolment refusing an Israeli ID.

## What has run

Priority 1 (GPE Open place, Nearby, Befriend) runs in the simulation under `:boot` and in one process over a network (`superapp_nearby_test`).  On the phones: both carry GrassApp 8031c235 since 14:45 UTC; Open place on both showed neither in Nearby; the 11 Pro's syslog shows it advertising over Bluetooth and nothing of a discovery (Code #26, GSA_inbox.md 15:44 UTC).  Not yet diagnosed: no log of either phone's layer exists until the reporter lands.

## Next

1. The log service and the reporter (`docs/debugging.md`), approved by Udi 15:19 UTC, tasked to Code 15:20 UTC, parcelled by Code to the clouds Debuglog and Reporter.
2. `p1-two-phones-lan` on the two phones, judged from their logs; then `p1-two-phones-ble`; then `p1-three-phones`, which waits on an introduction on home (GSA_inbox.md 15:48 UTC).
