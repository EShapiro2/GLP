# The figures of CSSN's paper

CSSN's paper copies these files into its own `figures/`.

## Figure 2: the village

`cssn-village.png` is drawn by `village.py` from the run of `play_village` (`../plays.glp`), one panel per agent: what each person requested and approved, and what the program wrote to them.  From the repository root:

```bash
python3 programs/cssn/figures/village.py cascade/CSSN/run.log
```

## Figure 3: the mini-app in the grassroots app

`cssn-app-1.png` to `cssn-app-6.png` are the six screens of Figure 3 (CSSN, Section 4.6, "In the Grassroots App"), in the scenario of four people, after GKK (CSSN #9 Cowork, Code_inbox.md, 2026-10-20 19:44 UTC, item 3): Carol enters with her parent Alice, and Dave with his parent Bob; Alice and Bob meet and befriend; Alice requests that Carol and Dave meet, naming Bob, and Bob, Carol and Dave approve; Alice requests that Carol and Dave befriend, naming Bob, and the three approve; Carol ends the friendship with Dave; and Bob declares Dave of age.  Each run has one person at their phone, whose screen is taken, and the other three scripted by `../person.glp` (the roles `fig_alice` to `fig_dave`); a scripted Alice looks at Carol's phone, which the test hands her, to request the befriending once Carol and Dave have met, her own state not changing by the meeting.

The app declares its widgets in `../self.vglp`: `Approval =::= approval_sentence.` and `Menu =::= social_menu.`, drawn by the construct family (`glp_multiagent/lib/ui_runtime/named_widgets.dart` and `tab_screen.dart`) in the words Udi gave for the figure (reported by CSSN #8 Cowork, Code_inbox.md, 2026-10-20 10:10 UTC) and the words CSSN gave for its own acts (CSSN #9 Cowork, Code_inbox.md, 2026-10-20 19:44 UTC, item 4), its meetings of a child and a child's joining of a group; GKK's meet and introduce are drawn in GKK's words, as in every app that extends GKK (GSC #5 Cowork, Code_inbox.md, 2026-10-20 20:21 UTC): Meet on a person's row, "Alice asks to meet you in person.", and "Introduce … to …." with Introduce.  Each person known is drawn with what the person's state says of them, read off the last view their agent wrote; and each request is a sentence the person completes by choosing a person into each place, and Submit, or Introduce for an introduction.

Each file is one phone's screen taken on the iOS Simulator, an iPhone 16e, 390 by 844 points at 3 pixels a point, its status bar at 9:41 with full signal, Wi-Fi and battery, in the light theme, set in Roboto.  The simulator's screenshot is the display alone, so it is framed as the phone, in a black bezel with rounded corners, 1080 by 2220 pixels on white, as the screens of the apps of GSC's contracts and GAI's are (`../../apps/*/screens/`).  From `glp_multiagent/`, under the Mac's run lock:

```bash
bash tool/sim_screens.sh
```

The script plays on the simulator the scenario of `glp_multiagent/test/apps_screens_test.dart` (`integration_test/apps_screens_test.dart`), the screens of the apps and these six, taking each person's acts by tapping on their phone's controls; at each screen the host takes the simulator's screenshot and `tool/sim_shot.py` frames it and writes it here.  The same scenario runs headless in the suite, `flutter test test/apps_screens_test.dart`: it takes every act and checks every screen, and writes nothing.

| File | What it shows |
|---|---|
| `cssn-app-1.png` | (1) Alice's menu, after she and Bob meet and befriend: the persons her agent knows, "Carol, your child", "Bob, your friend" with End friendship, and "Dave", whom Bob's agent introduced to hers, with Meet, she not having met him; under them the requests she may make, "Introduce … to …." with Introduce, "Ask that … and … meet.", "Ask that … and … meet, with …." and "Ask that … and … befriend, with ….", each with Submit. |
| `cssn-app-2.png` | (2) The same screen after Carol and Dave meet, as Alice composes her request: "You asked that Carol and Dave meet, with Bob." over a fresh "Ask that … and … meet, with ….", and "Ask that Carol and Dave befriend, with Bob." and Submit, which proposes `child_befriend(carol, dave, alice, bob)`. |
| `cssn-app-3.png` | (3) Bob's Friends tab, badged: "Alice asks that their child Carol and your child Dave befriend.", "Approve?", Yes and No at its top; under it "Dave, your child", "Alice, your friend" with End friendship, and "Carol" with Meet, and his requests. |
| `cssn-app-4.png` | (4) Carol's Friends tab, badged: "Alice asks that you and Dave befriend, with Dave's parent Bob.", "Befriend Dave?", Yes and No at its top; under it "Alice, your parent", "Bob" and "Dave", and "Introduce … to …." with Introduce. |
| `cssn-app-5.png` | (5) Carol's menu while Dave is her friend: "Alice, your parent", "Dave, your friend" with End friendship, and "Bob", whom Alice's agent introduced to hers; "Introduce … to …." with Introduce, the one request a child's state allows; and Add a person by key. |
| `cssn-app-6.png` | (6) Bob's menu as he composes his request, after Carol ends her friendship with Dave: "Declare Dave of age." and Submit, which proposes `of_age(bob, dave)`; nobody is asked, and once sent, "Dave is of age." |
