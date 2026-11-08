# p1-two-phones-ble: two phones befriend over Bluetooth

Phones #1 Cowork, 2026-11-08.  Priority 1 (Udi, 2026-11-07 22:54 UTC), the Bluetooth half.  The devices are the two iPhones, the 11 Pro and the 16 Pro, Alice's and Bob's; the Mac has no Bluetooth role in the app.  The script and the expectations are those of `p1-two-phones-lan.md`, with the differences below; what must happen is GSA's Appendix "The Operating System", "Nearby", and GNA's Section "PAN".

## Network

Wi-Fi off on both phones, so that only Bluetooth carries the play; Bluetooth on; cellular on, for the log.  Both apps in the foreground: iOS advertises the service to strangers only in the foreground (GNA, "Advertising and Recognition").

## Script

As `p1-two-phones-lan.md`, steps 1 to 7, and then:

8. Alice: put the app in the background for a minute, then bring it back.

## Expectations

As `p1-two-phones-lan.md`, with `pan` in place of `lan` throughout, and:

3. The discovery is a BLE scan match of the Grassroots prefix (GNA, "Advertising and Recognition"), an unrecognised presence under open trust, ANNOUNCE over GATT and the XX handshake; the pair converges to two GATT legs, the iOS phone opening the first (GNA, Implementation Notes, "Dual-role BLE"); `onPeerConnected` on `pan`; the bound is 60 s, Bluetooth discovery being slower than mDNS.
7. After the restart, under closed trust, the recognition token in the service UUID is matched by the phone that knows the key, and the IK handshake follows with no ANNOUNCE (GNA, "Connection and Identity").
8. While Alice's app is in the background, Bob's `layer` keeps the session or reports `onPeerDisconnected` on `pan` and then, on her return, `onPeerConnected` again, the friend channel's messages waiting meanwhile (GSA, "Reconnection": a message to a friend with no path waits until there is one); neither `runtime` reports an error.  Which of the two the layer does is recorded for GNA's Implementation Notes.

## Known at writing

- The GATT path of the handshake and the attestation was tested at the protocol level only (GNA, Implementation Notes, "Deviations").
- The 16 Pro has no build at writing; the play waits for it.
