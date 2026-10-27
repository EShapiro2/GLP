# The screens of GFF's app

The screens of GFF's app, taken for GFF's paper (Code_inbox.md, GFF #1 Cowork, 2026-10-27 18:00 UTC), in the scenario of the app's play, `play_feeds` (`../plays.glp`, `../README.md`): Alice, Bob, Carol, Dave and Eve, the friendships Alice and Bob, Bob and Carol, Carol and Dave, Dave and Eve, Alice's posts, Bob's following of Alice, Bob's, Carol's and Dave's introductions of Alice, and Carol's, Dave's and Eve's asking and following through.  The app is compiled from GFF's contract and pragmas, `programs/gff/GFF.txt` and `GFF.glp`, with those of GSG and GKK, which it extends, on the cascade's run without the second signature (`cascade/GFF/2026-10-27-2`, GLP `a57bcdeb`).  Each file is one phone's screen taken on the iOS Simulator, an iPhone 16e, 390 by 844 points at 3 pixels a point, its status bar at 9:41 with full signal, Wi-Fi and battery, in the light theme, set in Roboto, framed as the phone in a black bezel with rounded corners, 1080 by 2220 pixels on white, as `../../gsnp/screens/README.md` describes.

They are taken by `glp_multiagent/tool/sim_screens.sh`, which plays on the simulator the scenario of `glp_multiagent/test/apps_screens_test.dart`, the test "GFF's app on five phones": it loads this app and runs its entry `miniapp/4` for the five over four conversations, and takes each person's acts by tapping and typing on the tabs' controls.  From `glp_multiagent/`:

```bash
bash tool/sim_screens.sh "iPhone 16e" "GFF"
```

The same scenario runs headless, `flutter test test/apps_screens_test.dart --plain-name GFF`: it takes every act and checks every screen, and writes nothing.

The words on the screens are GFF.txt's pragmas, with GSG's and GKK's: the tabs Contacts, Friends and Feeds, the last from "Feeds: the agents Alice follows."; the buttons and the fields of "Feed: Alice [Post] <item>", "Following: Alice [Follow] <person>", "Alice [Unfollow]" and "Follower discovery: Alice [Ask] <person>", with GSG's Unfriend and GKK's Acquaint and Introduce.  A row of Feeds is an agent the person follows, and under it the atoms of the person's state naming that agent: the friend followed through, each item of the agent's feed the person holds, drawn with its signatures, the author first and then each agent it passed through, in order, and the answer to the person's question, the friend who answered and the item that is their path.

Taken on 2026-10-27, on branch GFF-shots off GLP `a57bcdeb`.

| File | What it shows |
|---|---|
| `gff-app-1.png` | Alice's Feeds tab after she posted two items: Post, GFF's "[Post] <item>", with its field item, and no row, since Alice follows no one and Feeds lists the agents she follows ("Feeds: the agents Alice follows."); her two items are in her state, `feed(x)`, and on none of her tabs. |
| `gff-app-2.png` | Carol's Friends tab as she follows Alice through Bob: in Bob's row, under it "Alice, First item (Alice)", Bob's answer to her question, the buttons Unfriend, Follow and Ask, and Follow's form open, its field person with Alice's key, alice, typed into it, and the button Follow, GFF's "[Follow] <person>".  The person is typed, the variable of follow_through not being required held at Carol's role (vGLP, Section 5.4, "The contract's words"). |
| `gff-app-3.png` | Carol's Feeds tab after she followed Alice through Bob: Post at its top, and Alice's row, under it Bob, the friend she follows Alice through; the two items Bob delivered, "Second item (Alice, Bob)" and "First item (Alice, Bob)"; and "Bob, First item (Alice)", Bob's answer to her question, with the item that is his path to Alice. |
| `gff-app-4.png` | Eve's Feeds tab after Alice posted the third item: Alice's row, under it Dave, the friend Eve follows her through; "Third item (Alice, Bob, Carol, Dave)", "Second item (Alice, Bob, Carol, Dave)" and "First item (Alice, Bob, Carol, Dave)", each signed by Alice at its posting and by Bob, Carol and Dave on its way; and "Dave, First item (Alice, Bob, Carol)", Dave's answer to her question. |

Screens 5 and 6 of the task are not taken.  Screen 5, Carol's Ask in Bob's row with Alice picked: the app draws Ask's field person as a field typed by key, where vGLP, Section 5.4, "The contract's words", has a variable that the proposer's role requires held picked among the items the viewer holds, and ask requires `knows(b)` at Alice's role.  Screen 6, Bob's Feeds tab after Carol ended her following: no Unfollow is drawn on Carol's phone, though her state holds `follows(alice, bob)` and `following(alice)`, which unfollow requires at her role (vGLP, Section 6.2, "The words a platform shows"), so the unfollowing is not played.  Both are reported to Code.
