# Interface descriptions (GCE)

An app's screens described in the UI language of the Grassroots UI paper, in the contract's own words, and
checked against the contract: complete (every act a person may take is offered) and visible (every fact
shown).

- `Friendship.ui`: Friendship, person first: everyone Alice knows on the start screen, and a screen for each.
- `Friendship.ui.json`: the same, compiled with the contract into what the app reads: each act by its schema,
  each place by its number in the act's request.  Printed by the UI compiler, `src/uic.js` of the language's
  prototype (`grassroots/ui` of `nimrodtalmon/talmonlab`); edit the `.ui` and print it again.

The app draws a description in place of its tabs where it is given one
(`glp_multiagent/lib/ui_runtime/described.dart`).  To see Friendship's on two phones, its agents GLP's, from
`glp_multiagent/`:

    flutter run -d macos -t lib/main_described.dart

## Walking contracts' apps

`glp_multiagent/test/described_village_test.dart` walks contracts' apps drawn from descriptions, three persons
each, every agent GLP's, and compares at every step what the screens offer with what the platform's request
widget offers.  An app directory for a contract of the cascade is made by
`python3 programs/gce/ui/tools/mkapp.py . NAME dir` (under `programs/apps/_ui/`, not committed); the list of
apps and descriptions is given by `DV_LIST`.
