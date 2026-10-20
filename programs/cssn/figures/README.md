# The figures of CSSN's paper

CSSN's paper copies these files into its own `figures/`.

## Figure 2: the village

`cssn-village.png` is drawn by `village.py` from the run of `play_village` (`../plays.glp`), one panel per agent: what each person requested and approved, and what the program wrote to them.  From the repository root:

```bash
python3 programs/cssn/figures/village.py cascade/CSSN/run.log
```

## Figure 3: the mini-app in the grassroots app

`cssn-app-1.png` to `cssn-app-6.png` are the six screens of Figure 3 (CSSN, Section 4.6, "In the Grassroots App"), in the scenario of four people: Carol enters with her parent Alice, and Dave with his parent Bob; Alice and Bob befriend; Alice requests that Carol and Dave befriend, naming Bob, and Bob, Carol and Dave approve; Carol ends the friendship with Dave; and Bob declares Dave of age.  Each run has one person at their phone, whose screen is taken, and the other three scripted by `../person.glp` (the roles `fig_alice` to `fig_dave`).

The app declares its widgets in `../self.vglp`: `Approval =::= approval_sentence.` and `Menu =::= social_menu.`, drawn by the construct family (`glp_multiagent/lib/ui_runtime/named_widgets.dart` and `tab_screen.dart`) in the words Udi gave for the figure (reported by CSSN #8 Cowork, Code_inbox.md, 2026-10-20 10:10 UTC).  Each person known is drawn with what the person's state says of them, read off the last view their agent wrote; and each request is a sentence the person completes by choosing a person into each place, and Submit.

Each file is one phone's screen taken on the iOS Simulator, an iPhone 16e, 390 by 844 points at 3 pixels a point, its status bar at 9:41 with full signal, Wi-Fi and battery, in the light theme, set in Roboto.  The simulator's screenshot is the display alone, so it is framed as the phone, in a black bezel with rounded corners, 1080 by 2220 pixels on white, as the screens of the apps of GSC's contracts and GAI's are (`../../apps/*/screens/`).  From `glp_multiagent/`, under the Mac's run lock:

```bash
bash tool/sim_screens.sh
```

The script plays on the simulator the scenario of `glp_multiagent/test/apps_screens_test.dart` (`integration_test/apps_screens_test.dart`), the screens of the apps and these six, taking each person's acts by tapping on their phone's controls; at each screen the host takes the simulator's screenshot and `tool/sim_shot.py` frames it and writes it here.  The same scenario runs headless in the suite, `flutter test test/apps_screens_test.dart`: it takes every act and checks every screen, and writes nothing.

| File | What it shows |
|---|---|
| `cssn-app-1.png` | (1) Alice's menu, after she and Bob befriend: the persons her agent knows, "Carol, your child", "Bob, your friend" with End friendship, and "Dave", whom Bob's agent introduced to hers, with Befriend; under them the requests she may make, "Ask that … and … befriend, with …." and "Declare … of age.", each with Submit, and "Enter …." with Submit under "You asked to enter Carol."; and Add a person by key. |
| `cssn-app-2.png` | (2) The same screen as Alice composes her request: "Ask that Carol and Dave befriend, with Bob." and Submit, which proposes `child_befriend(carol, dave, alice, bob)`; once sent, "You asked that Carol and Dave befriend, with Bob." |
| `cssn-app-3.png` | (3) Bob's Friends tab, badged: "Alice asks that her child Carol and your child Dave befriend.", "Approve?", Yes and No at its top; under it "Dave, your child", "Alice, your friend" with End friendship, and "Carol" with Befriend, and his requests. |
| `cssn-app-4.png` | (4) Carol's Friends tab, badged: "Alice asks that you and Dave befriend, with Dave's parent Bob.", "Befriend Dave?", Yes and No at its top; under it "Alice, your parent" and "Bob". |
| `cssn-app-5.png` | (5) Carol's menu while Dave is her friend: "Alice, your parent", "Dave, your friend" with End friendship, and "Bob", whom Alice's agent introduced to hers; and Add a person by key.  A child composes no request. |
| `cssn-app-6.png` | (6) Bob's menu as he composes his request, after Carol ends her friendship with Dave: "Declare Dave of age." and Submit, which proposes `of_age(bob, dave)`; nobody is asked, and once sent, "Dave is of age." |
