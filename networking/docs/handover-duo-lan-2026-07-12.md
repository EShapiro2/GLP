# Handover — GrassApp Duo on two physical iPhones over LAN

**Date:** 2026-07-12.  **From:** the networking code session that implemented LAN discovery, the core split, and the headless embedding.  **Goal:** the two phones discover each other and pair directly on a LAN — no rendezvous anchor — per spec §LAN (`GLP-Networking-API/sections/proximity.tex`).

Read `/Users/udi/Grassroots/GLP/CLAUDE.md` and the paper-code map first, as always.

## Where things stand

- LAN discovery is implemented and committed: DNS-SD over mDNS via `bonsoir`, service type `_grassroots._udp`, rotating token = first 8 bytes hex of SHA-256("glp lan suffix" | pk | slot), 15-minute slots, adjacent-slot matching (`core/lib/src/models/identity.dart`, `lib/src/transport/lan_discovery_service.dart`, coordinator wiring in `lib/src/grassroots_network.dart`). `NSBonjourServices` + local-network usage strings are in both apps' Info.plist.
- Both phones have the duo **installed and launched once**, but from an OLD build (commit `d4bd1cde`). Since then the layer changed substantially (core package split, Noise ordering fixes, per-pair handshake serialization, uniform Closed-trust gate over IP — commits `8b9fdfd1..0357290f`). **Rebuild and reinstall both phones before testing.**
- The status banner on launch reads `starting…` → `pairing…` → `linked (…)` on success. Each phone `putKnownPeer`s the other at startup (fixed seeds), so Closed trust admits them; the anchor is exempt as a configured rendezvous server.
- iOS will show a **Local Network permission dialog** on first mDNS use — Udi must tap Allow on both phones.

## Blocker that ended the last attempt

Udi's WiFi has **client isolation**: phones reached the Mac-hosted anchor outbound, but phone↔phone and Mac→phone traffic was 100% blocked. It likely blocks mDNS multicast too. The test needs a proper LAN: a home router without isolation, or a hotspot from a **third** device. If banners sit at `pairing…` for over a minute after Allow, suspect the network, not the code.

## Devices

| Phone | Role | flutter device id | devicectl UUID | Notes |
|---|---|---|---|---|
| iPhone 16 Pro ("iUdi 16pro") | `phone1` (bob + eve) | `00008140-001C38C83A12801C` | `2219A3DC-78B5-5721-9E5E-FC2686D6A402` | iOS 26.5 |
| iPhone 11 Pro Max ("iUdi 11pro") | `phone2` (alice + dana) | `00008030-000275600A12802E` | `C1F923A2-54D9-54DA-912D-8D033E7E425F` | iOS 17.4.1; Developer Mode on; provisioning registered |

Bundle id `com.eshapiro.grassapp`. Launch after install: `xcrun devicectl device process launch --device <UUID> com.eshapiro.grassapp`.

## Rebuild + install (per phone; run from `/Users/udi/Grassroots/GLP/glp_multiagent`)

First `bash tool/sync_glp_assets.sh` (bundles `play_grassapp_duo.glp`), then per role:

```
flutter build ios --release -t lib/main_grassapp_duo.dart \
  --dart-define=DUO_ROLE=phone1 \
  --dart-define=DUO_ANCHOR_ADDR=<mac-lan-ip>:9516 \
  --dart-define=DUO_ANCHOR_PK=b83e2276d995c0a9afd74c939bc9390b2d3a3606f9110e5a5c91b9b62601d350
xcrun devicectl device install app --device 2219A3DC-78B5-5721-9E5E-FC2686D6A402 build/ios/iphoneos/Runner.app
```

Repeat with `DUO_ROLE=phone2` and the 11 Pro's UUID. The build output directory is reused — install phone1's app before building phone2's.

The anchor defines are an optional fallback (unreachable under client isolation; unnecessary on a working LAN). Anchor: `dart run bootstrap_anchor` (in `GLP/networking/bootstrap_anchor`) with identity file `{"seed":"7a8e99d6182579f0c8d9b39cca74c1bfd49bcfd796d53d9e47c4b8c92db2812b","nickname":"rendezvous"}`; its pubkey is the `DUO_ANCHOR_PK` above; the Mac's LAN IP was 192.168.1.17 (re-check with `ifconfig en0`).

## Suggested sequence

1. **Simulator sanity first** (the layer changed a lot since the duo last ran): boot two iPhone simulators, run the duo as phone1/phone2 the same way (`flutter run`/`flutter test` targeting simulators, or install the .app), confirm they reach `linked` on localhost. This also exercises the new trust gates end-to-end in the duo.
2. Rebuild + reinstall both phones (above).
3. Put both phones on a non-isolated LAN, launch, tap Allow on the Local Network prompts.
4. Success = both banners `linked (MessageTransport.udp)` and the play's cross-phone messages flowing (bob↔alice chat surfaces). LAN log lines (`[lan] advertising …`, `[lan] recognized known peer …`) are visible when running via `flutter run` attached, or in Console.app filtered on `Runner`.

## Gotchas learned the hard way

- Two concurrent `flutter build`/`flutter run` invocations clobber each other's dart-defines — build/install roles **serially**.
- The 11 Pro needed the provisioning profile UDID registered; `flutter run --release -d 00008030-000275600A12802E -t lib/main_grassapp_duo.dart --dart-define=…` did the registration; on-phone developer trust plus Developer Mode toggle were also needed (all done — should not recur).
- Kill stale simulator apps before any anchor-mediated test: dead simulators with the same seeded identities poison the anchor's address table.
- LAN token rotation: if one phone sat launched across a 15-minute slot boundary while the other did not, the adjacent-slot match covers it; a `[lan] unmatched instance` line with a plausible token usually means clock skew beyond ±1 slot — check both phones' clocks.
