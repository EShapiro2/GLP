# p1-two-phones-lan: two devices befriend over the local network

Phones #1 Cowork, 2026-11-08.  Priority 1 (Udi, 2026-11-07 22:54 UTC), the LAN half.  The devices are Alice's and Bob's; the first run takes the Mac as Alice and the 11 Pro as Bob, and a later run the two phones.  What must happen is GSA's Appendix "The Operating System", "Nearby" and "The channel to a person one knows", GPE and GSG, and GNA's Sections "Proximity Underlays" and "Session Establishment"; the screens are those Code reports for GLP main 203632c2 and later (GSA_inbox.md, 2026-11-08 08:03 and 08:53 UTC).

## Network

Both devices on one local network without client isolation, a hotspot from a third device if Udi's Wi-Fi isolates; Bluetooth off on both, so that only the LAN carries the play.  Both devices have a path to the Internet for the log, cellular on a phone.

## Script

1. Alice: open the app.  Bob: open the app.
2. Alice: Open place.
3. Bob: Open place.
4. Alice: Befriend, on Bob's card in Nearby.
5. Bob: yes.
6. Alice: Close place.  Bob: Close place.
7. Both: close the app and open it again.

## Expectations

After each step, in each device's log, in this order, within the bound given; a key is the other device's.

1. Within 30 s of opening: `app` reports the build and the device; `runtime` reports the agent started, restored or new, and the first act `superapp(Me, Known)`; `layer` reports both proximity trusts closed and `nearby` declared for `pan` and `lan`, and every known person declared a known peer; `screen` shows home with the menu trust, install, invite, Open place, Close place, and no card.  Nothing is discovered: both trusts are closed and the two do not know each other.
2. Within 5 s: Alice's `layer` sets `pan` and `lan` open; her `screen` shows the place open.  Bob's log shows nothing of Alice.
3. Within 30 s: Bob's `layer` sets both trusts open.  Then, on both: `layer` discovers the other on `lan` (an unrecognised instance under open trust, ANNOUNCE, the XX handshake, GNA "Session Establishment"); `onPeerConnected` on `lan`, the application `null`, both being unattested; `runtime` at the device with the smaller key puts GPE:2, and the other answers yes with no question asked; both `runtime`s report the other known, and `layer` on both declares the other a known peer; `screen` on both shows the other's card in Nearby with Befriend, and no question.
4. Within 5 s: Alice's `runtime` sends GSG:1's start to Bob; Bob's `runtime` receives it; Bob's `screen` asks him "Alice asks to be your friend" with yes and no.
5. Within 10 s: Bob's `runtime` commits GSG:1 and sends the outcome; Alice's receives it; both `runtime`s report `connected(<key>)`; both `screen`s show the other as a friend and Bob's question gone; the friend channel's handshake, each telling the other its earliest compatible version, where the operating system has it (GSA, Appendix, "The channel to a person one knows"; not built at 2026-11-08 00:29 UTC, Code's point 5, so this expectation is recorded and not yet required).
6. Within 5 s: both `layer`s set both trusts closed; `onPeerConnected` is not withdrawn: the two stay connected over `lan`, known peers being recognised under closed trust.
7. Within 30 s of reopening: `runtime` reports the agent restored; `screen` shows the other as a friend; `layer` declares the other a known peer at start, discovers them under closed trust by the recognition token and connects with the IK handshake (GNA "Proximity Underlays", "Contact and identity"); the friendship made survived the restart (Code, 2026-11-08 11:39 UTC).

## Known at writing

- A card stays on home after its person leaves or after the other befriended first (vGLP's, Code 08:53 UTC): not a fault of this play.
- Both devices unattested: the app has no attestation producer (Code 00:53 UTC, point 2); `peer_app/2` is not checked by core.
- The Mac app showed no window on 2026-11-08 (Code #25, 13:38 UTC); until it does, the play waits for the 16 Pro.
