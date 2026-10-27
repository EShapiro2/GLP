# The screens of GFF's app

The screens of GFF's app, taken for GFF's paper (Code_inbox.md, GFF #1 Cowork, 2026-10-27 18:00, 18:55 and 20:25 UTC), in the scenario of the app's play, `play_feeds` (`../plays.glp`, `../README.md`): Alice, Bob, Carol, Dave and Eve; the friendships Alice and Bob, Bob and Carol, Carol and Dave, Dave and Eve, friends following each other; Alice's posts; Bob's, Carol's and Dave's introductions of Alice; and Carol's, Dave's and Eve's following of Alice, each a tap on Follow, their machines asking every friend and following through one who follows her.  The app is compiled from GFF's contract and pragmas, `programs/gff/GFF.txt` and `GFF.glp` at GLP `56270cb6`, with those of GSG and GKK, which it extends (`cascade/GFF/2026-10-27-3`, at `0a3d7997`, the contract before its Posts line named feed).  Each file is one phone's screen taken on the iOS Simulator, an iPhone 16e, 390 by 844 points at 3 pixels a point, its status bar at 9:41 with full signal, Wi-Fi and battery, in the light theme, set in Roboto, framed as the phone in a black bezel with rounded corners, 1080 by 2220 pixels on white, as `../../gsnp/screens/README.md` describes.

They are taken by `glp_multiagent/tool/sim_screens.sh`, which plays on the simulator the scenario of `glp_multiagent/test/apps_screens_test.dart`, the test "GFF's app on five phones": it loads this app and runs its entry `miniapp/4` for the five over four conversations, and takes each person's acts by tapping, picking and typing on the tabs' controls.  From `glp_multiagent/`:

```bash
bash tool/sim_screens.sh "iPhone 16e" "GFF"
```

The same scenario runs headless, `flutter test test/apps_screens_test.dart --plain-name GFF`: it takes every act and checks every screen, and writes nothing.

The words on the screens are GFF.txt's pragmas, with GSG's and GKK's: the tabs Contacts, Friends, Posts and Feeds, from "Posts: Alice's feed." and "Feeds: the agents Alice follows."; and the buttons of "Feed: Alice [Post] <item>" and "Following: Alice [Follow]" and "  Alice [Unfollow]", with GSG's Befriend and GKK's Acquaint and Introduce.  A row of Feeds is an agent the person follows, and under it, in the order of the person's state, the atoms of that state naming the agent, each by its other arguments: the friend the person follows the agent through; each item of the agent's feed the person holds, drawn with its signatures, the author first and then each agent it passed through; each friend asked whether they follow the agent; and each answer, a friend and the item that is their path to the agent, or a friend who does not follow the agent.

Taken on 2026-10-27, on branch GFF-shots2, screens 2 to 7 off GLP `a8b2f179` and screen 1 after the merge of `56270cb6`, at which 2 to 7 came out byte for byte the same.

| File | What it shows |
|---|---|
| `gff-app-1.png` | Alice's Posts tab after her two posts: Post, GFF's "[Post] <item>", with its field item, and a row for each item she posted, the newest first, "Second item" and "First item", under each Alice, its signer, and Bob, the friend her machine sent it to. |
| `gff-app-2.png` | Carol's Contacts tab after Bob introduced Alice to her: her name, Acquaint with its field person, and a row for each person she knows, Alice, Dave and Bob; in Alice's row Introduce, Befriend and Follow, GFF's "[Follow]", which she taps to follow Alice, with no field. |
| `gff-app-3.png` | Carol's Feeds tab after she followed Alice: Alice's row, under it Bob, the friend she follows Alice through; Bob's answer, "Bob, Second item (Alice)", his path to Alice; the two items Bob delivered, "First item (Alice, Bob)" and "Second item (Alice, Bob)"; Bob and Dave, the friends her machine asked; and Dave, who answered that he does not follow Alice.  Below, Dave's and Bob's rows, the friends she follows by friendship, and Unfollow in Alice's row. |
| `gff-app-4.png` | Eve's Feeds tab after Alice posted the third item: Alice's row, under it Dave, the friend Eve follows Alice through; "Third item (Alice, Bob, Carol, Dave)", "First item (Alice, Bob, Carol, Dave)" and "Second item (Alice, Bob, Carol, Dave)", each signed by Alice at its posting and by Bob, Carol and Dave on its way; Dave's answer, "Dave, Second item (Alice, Bob, Carol)"; and Unfollow; below, Dave's row, her friend. |
| `gff-app-5.png` | Dave's Feeds tab after he followed Alice: Alice's row, under it Carol, the friend he follows Alice through; Carol's answer, "Carol, First item (Alice, Bob)", her path to Alice; the items Carol delivered, signed by Alice, Bob and Carol; Carol and Eve, the friends his machine asked; and Eve, who answered that she does not follow Alice; and Unfollow; below, Eve's and Carol's rows, his friends. |
| `gff-app-6.png` | Carol's Feeds tab before she ends her following: Alice's row with the third item, "Third item (Alice, Bob)", and Unfollow, GFF's "[Unfollow]", at its foot. |
| `gff-app-7.png` | Carol's Feeds tab after she tapped Unfollow: Alice's row gone, and Dave and Bob, her friends, whom she follows by friendship. |

Screens 3 and 5 of the task both name Carol's Feeds tab; screen 3 is it, and screen 5 is Dave's, which shows the path answer one hop further.  Screen 6 of the task is two files, 6 before the unfollowing and 7 after it.
