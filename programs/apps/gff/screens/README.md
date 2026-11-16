# The screens of GFF's app

The screens of GFF's app, taken for GFF's paper (Code_inbox.md, GFF #6 Cowork, 2026-11-15 21:54 UTC), in the scenario of the app's play, `play_feeds` (`../plays.glp`, `../README.md`), its steps 1 to 8: Alice, Bob, Carol and Dave; the friendships Alice and Bob, Bob and Carol, Carol and Dave; Alice's two posts; Carol's pointing through Bob at Alice; Carol's introductions of Bob and Dave to each other; Dave's pointing through Carol at Alice; Carol's ending of her pointing; and Alice's third and fourth items.  The app is compiled from GFF's contract and pragmas, `programs/gff/GFF.txt`, as adopted from the Calculus, with those of the contracts it extends.  Each file is one phone's screen taken on the iOS Simulator, an iPhone 16e, 390 by 844 points at 3 pixels a point, its status bar at 9:41 with full signal, Wi-Fi and battery, in the light theme, set in Roboto, framed as the phone in a black bezel with rounded corners, 1080 by 2220 pixels on white, as `../../gsnp/screens/README.md` describes.

They are taken by `glp_multiagent/tool/sim_screens.sh`, which plays on the simulator the scenario of `glp_multiagent/test/apps_screens_test.dart`, the test "GFF's app on four phones": it loads this app and runs its entry `miniapp/3` for the four over three conversations, and takes each person's acts by tapping, picking and typing on the tabs' controls.  From `glp_multiagent/`:

```bash
bash tool/sim_screens.sh "iPhone 16e" "GFF"
```

The same scenario runs headless, `flutter test test/apps_screens_test.dart --plain-name GFF`: it takes every act and checks every screen, and writes nothing.

The words on the screens are GFF.txt's pragmas, with those of the contracts it extends: the tabs Contacts, Friends, Pointing, Posts and Feeds, and the buttons Post, Labeled point and Labeled unpoint, with Befriend, Unfriend, Acquaint and Introduce.  A row of Pointing is a person the viewer points through, and under it the person pointed at, a friend being pointed at through himself; a row of Posts or Feeds is an item, and under it its signer.

Taken on 2026-11-16 at GLP main `8f1f1f57`, the scenario in a simulator run of its own.

| File | What it shows |
|---|---|
| `gff-app-1.png` | Alice's Posts tab after her two posts: a row for each item she posted, the newest first, "Second item" and "First item", under each Alice, its signer. |
| `gff-app-2.png` | Carol's Pointing tab after she pointed through Bob at Alice, Labeled point beside Bob with alice typed into its field: a row Bob, under it Alice, the person she points at through him, with Labeled point and Labeled unpoint; and a row for each friend, Dave and Bob, under each his own name, with Unfriend, Labeled point and Labeled unpoint. |
| `gff-app-3.png` | Carol's Feeds tab after she pointed at Alice: Alice's two items, "Second item" and "First item", under each Alice. |
| `gff-app-4.png` | Dave's Contacts tab after the two introductions, Bob to Dave and Dave to Bob, each by Carol: his name; Acquaint with its field person; Post with its field item; Bob's row with Introduce, Befriend and Labeled point; and Carol's row with Introduce. |
| `gff-app-5.png` | Dave's Pointing tab after he pointed through Carol at Alice, picking Carol's pointer: a row Bob, under it Alice, and a row Carol, under it Alice, each with Labeled unpoint; and a row Carol, under it Carol, his friend, with Unfriend, Labeled point and Labeled unpoint. |
| `gff-app-6.png` | Carol's Pointing tab after she tapped Labeled unpoint beside her pointer through Bob at Alice: the pointer's row gone, and Dave and Bob, her friends, each with Unfriend, Labeled point and Labeled unpoint. |
| `gff-app-7.png` | Dave's Feeds tab after Alice's third and fourth items: "Fourth item", "Third item", "Second item" and "First item", under each Alice; the fourth reached Dave and not Carol, who had ended her pointing. |

Screens 3 and 5 of the task both name Carol's Feeds tab; screen 3 is it, and screen 5 is Dave's, which shows the path answer one hop further.  Screen 6 of the task is two files, 6 before the unfollowing and 7 after it.
