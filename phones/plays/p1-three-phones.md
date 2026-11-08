# p1-three-phones: a third device befriends one and is introduced to the other

Phones #1 Cowork, 2026-11-08.  Priority 1 (Udi, 2026-11-07 22:54 UTC), "then three".  The devices are Alice's and Bob's, already friends by `p1-two-phones-lan.md` or `p1-two-phones-ble.md`, and Carol's, the third: the Mac, or a third phone when there is one.  What must happen is GSA's Appendix "The Operating System", "Nearby" and "Messages", GPE:2, GSG:1 and GKK:3, and the three-agent play under `:boot` (Code, 2026-11-08 00:29 UTC: Carol befriends one and is introduced to the other).

## Network

The local network of `p1-two-phones-lan.md`, all three on it; or Bluetooth where all three have it.  Each device has a path to the Internet for the log.

## Script

1. Alice, Bob, Carol: open the app; Alice and Bob are friends from the earlier play.
2. Carol: Open place.  Bob: Open place.
3. Carol: Befriend, on Bob's card in Nearby.
4. Bob: yes.
5. Bob: Introduce Alice and Carol, where home offers it; otherwise the introduction is Code's to show and this step is recorded as not available.
6. Alice: yes.  Carol: yes.
7. All: Close place.

## Expectations

1. As `p1-two-phones-lan.md`, step 1, on three devices; Alice's and Bob's `screen`s show each other as friends; nobody is discovered, every trust being closed.
2. Within 30 s: Carol's and Bob's `layer`s open both trusts; each discovers the other; the device with the smaller key puts GPE:2 and the other answers yes; both know each other and each `screen` shows the other's card in Nearby.  Alice's log shows nothing of Carol: her place is closed and she does not know Carol.
3. and 4. As `p1-two-phones-lan.md`, steps 4 and 5, between Carol and Bob: GSG:1 committed, `connected` on both.
5. Within 5 s: Bob's `runtime` sends GKK:3's start to Alice and to Carol over his two friend channels (GSA, Appendix, "Messages": an introduction's message goes over the introducer's channels); Alice's and Carol's `screen`s each ask whether to know the other.
6. Within 10 s: on each yes, that `runtime` reports the other known and `layer` declares the other a known peer (GSA 4.2: Alice's yes to the act is her yes to knowing the agent); GKK:3 commits at all three; Alice and Carol each hold a channel to the other, made by the smaller key and sent over Bob's channels (GSA, Appendix, "The channel to a person one knows"); Alice's and Carol's `screen`s show each other in Nearby, known and not yet friends, where they are on one network, and Bob takes no further part.
7. Within 5 s: every `layer` closes both trusts; the three stay connected, as known peers.

## Known at writing

- Under `:boot` the three-agent play passes 40 of 40 since IGLP 45fabfc (Code, 2026-11-08 08:53 UTC); in the simulation Alice and Bob know each other from the first act, which on devices they do only by the earlier play.
- Whether home offers an introduction to a person's friends is GSA's: the paper's 4.2 has it, and Code's report of 08:03 UTC lists home's menu as trust, install, invite, Open place, Close place.  Where it is missing, the fault goes to GSA, the paper deciding, and the play stops at step 5.
