# The figures of CSSN's paper

CSSN's paper copies these files into its own `figures/`.

## Figure 2: the village

`cssn-village.png` is drawn by `village.py` from the run of `play_village` (`../plays.glp`), one panel per agent: what each person requested and approved, and what the program wrote to them.  From the repository root:

```bash
python3 programs/cssn/figures/village.py cascade/CSSN/run.log
```

## Figure 3: the mini-app in the grassroots app

`cssn-app-1.png` to `cssn-app-6.png` are the six screens of Figure 3 (CSSN, Section 4.6, "In the Grassroots App"), in the scenario of four people, after GKK (CSSN #9 Cowork, Code_inbox.md, 2026-10-20 19:44 UTC, item 3), on the contract with any number of parents at ddc1f4cc (CSSN #10 Cowork, Code_inbox.md, 2026-10-24 17:29 UTC, item 3): Alice enters Carol and Bob enters Dave, each the child's creator; Alice and Bob meet and befriend; Alice requests that Carol and Dave meet, naming Bob, and Bob, Carol and Dave approve; Alice requests that Carol and Dave befriend, naming Bob, and the three approve; Carol ends the friendship with Dave; and Bob declares Dave of age.  Each run has one person at their phone, whose screen is taken, and the other three scripted by `../person.glp` (the roles `fig_alice` to `fig_dave`); a scripted Alice looks at Carol's phone, which the test hands her, to request the befriending once Carol and Dave have met, her own state not changing by the meeting.

The app is compiled from the contract (claude.md, "Sessions": "THE CONTRACT IS THE SOURCE OF ALL TRUTH RELATED TO THE APP"): vGLP's compiler prints into the platform (`../platform.vglp`) the two widget declarations, `Approval =::= approval_sentence.` and `Menu =::= social_menu(...)`, the latter carrying the pragmas of `../CSSN.txt` and of the contracts CSSN extends, GKK, GSG, GSN and GnW (vGLP, Section 5.4, "The contract's words"; Section 6.2, "The words a platform shows"), and the construct family (`glp_multiagent/lib/ui_runtime/named_widgets.dart` and `tab_screen.dart`) draws them: a tab for each list the pragmas name, Contacts, Friends, Groups and Children, each listing the atoms of its predicate in the person's state, a row showing the atoms naming its item; beside a row the button of every act that names a person, at each of its guarding roles, on whichever tab lists the person, labelled as the pragma labels it (vGLP 110caf4, Section 6.2, "The words a platform shows"), so that a row of a child shows Meet, Introduce, Befriend, Unfriend, Message, Enter, Make parent, End parent, Of age, Retire, Child meet (child_meet's and child_meet_2's), Child befriend, Child adult befriend, Child unfriend and Child join (child_join's and child_join_2's), and a row of a friend Join besides; the other persons of an act picked into its places, each named by its type, Peer, where no pragma names it; Enter, a meeting schema, with a field for the child's key, CSSN's `<child>`; an approval question as the question of the role asked, "Alice asks that Carol and your child Dave befriend.", with yes and no, at the top of the tab of the act's contract; and the question a hello asks, "Alice asks to be your parent.", likewise.  Every word on the screens is the contract's, but yes and no, the platform's, and Peer, the type's name.

Each file is one phone's screen taken on the iOS Simulator, an iPhone 16e, 390 by 844 points at 3 pixels a point, its status bar at 9:41 with full signal, Wi-Fi and battery, in the light theme, set in Roboto.  The simulator's screenshot is the display alone, so it is framed as the phone, in a black bezel with rounded corners, 1080 by 2220 pixels on white, as the screens of the apps of GSC's contracts and GAI's are (`../../apps/*/screens/`).  From `glp_multiagent/`, under the Mac's run lock:

```bash
bash tool/sim_screens.sh
```

The script plays on the simulator the scenario of `glp_multiagent/test/apps_screens_test.dart` (`integration_test/apps_screens_test.dart`), the screens of the apps and these six, taking each person's acts by tapping on their phone's controls; at each screen the host takes the simulator's screenshot and `tool/sim_shot.py` frames it and writes it here.  The same scenario runs headless in the suite, `flutter test test/apps_screens_test.dart`: it takes every act and checks every screen, and writes nothing.

| File | What it shows |
|---|---|
| `cssn-app-1.png` | (1) Alice's Children tab, after she and Bob meet and befriend: Enter with its field child, and Carol, her child, with the button of every act that names a person (above), Make parent, End parent, Of age and Retire among them.  Re-taken on the iOS Simulator on branch CSSN-many, 2026-10-24.  Dave, whom Bob's agent introduced to hers, is on no tab, her state naming him in no atom; he is offered where she picks a person into a place. |
| `cssn-app-2.png` | (2) The same tab after Carol and Dave meet, as Alice composes her request: Child befriend open beside Carol, Dave picked into the second child and Bob into the other parent, each place named Peer, and the button Child befriend, which proposes `child_befriend(carol, dave, alice, bob)`.  Re-taken on the iOS Simulator at 2426effd, on main's compiler: two pickers, each named Peer, the second child offering Bob and Dave and the other parent offering Bob alone, the one row of Alice's Friends, since that place is an item of the list of friend, `item(4, friend)`, and is picked once, from that list's rows. |
| `cssn-app-3.png` | (3) Bob's Children tab, badged: "Alice asks that Carol and your child Dave befriend.", yes and no at its top; under it Enter, and Dave, his child, with his buttons. |
| `cssn-app-4.png` | (4) Carol's Children tab, badged: "Alice asks that you and Dave befriend.", yes and no at its top; the tab lists no child of hers. |
| `cssn-app-5.png` | (5) Carol's Friends while Dave is her friend: Dave, and Alice, her parent and first friend, each with the buttons of every act that names a person, Unfriend and Join among them.  Bob, whom Alice's agent introduced to hers at the level of the machines, is on no tab. |
| `cssn-app-6.png` | (6) Bob's Children tab after Carol ends her friendship with Dave: Enter, and Dave, his child, with his buttons, Of age among them, CSSN's "[Of age]", which proposes `of_age(bob, dave)` when tapped, Bob being Dave's creator; nobody is asked, Bob having made no other parent of Dave, and Dave then leaves the tab.  Re-taken on the iOS Simulator on branch CSSN-many, 2026-10-24. |
