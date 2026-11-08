# Phones --- state, 2026-11-08 15:20 UTC (Phones #1 Cowork)

Taken over from `tmp/phones/STATE.md` (Code #25B Phones, 13:57 UTC) and Code #25's handover (Code_inbox.md, 13:36 and 13:38 UTC).

## Devices

- 11 Pro: UDID `00008030-000275600A12802E`, iOS 17.4.1; GrassApp at GLP main 8031c235 installed 13:08 UTC by `ideviceinstaller`, developer trusted, opened; signed for it by the free team G374822FDT.
- 16 Pro: UDID `00008140-001C38C83A12801C`, iOS 26.6.1; no GrassApp; Xcode stuck "Preparing iUdi 16pro" since 13:21 UTC; a build signed for it waits on Xcode accepting the phone; Udi asked to restart it.
- The Mac as an agent: a team-signed debug build with the Keychain entitlement starts but shows no window (Code #25, 13:38 UTC); the entitlement edit is not in the tree.

## Signing

The free team signs one phone per build; the paid account waits on Apple Developer Support, Apple's enrolment refusing an Israeli ID.

## What has run

Priority 1 (GPE Open place, Nearby, Befriend) runs in the simulation under `:boot` and in one process over a network (`superapp_nearby_test`); nothing has run on two devices.

## Next

1. The log service and the reporter (`docs/debugging.md`), tasked to Code once Udi approves the three points there.
2. `p1-two-phones-lan` on the Mac app and the 11 Pro; then the 16 Pro and Bluetooth; then three.
