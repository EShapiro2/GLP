# Interface descriptions (GCE)

An app's screens described in the UI language of the Grassroots UI paper, in the contract's own words, and
checked against the contract: complete (every act a person may take is offered) and visible (every fact
shown).

- `Friendship.ui`: Friendship, person first: everyone Alice knows on the start screen, and a screen for each.
- `Friendship.ui.json`: the same, compiled with the contract into what the app reads: each act by its schema,
  each place by its number in the act's request, the proposer's place the platform's.  Edit the `.ui` and print
  it again.

## The tools (`tools/`, Node and Python 3, no Dart)

- `build.py CALC NAME OUT.json`: the contract as the tools read it, from a scratch copy of `calculus/` made as
  `calculus/tools/SAS-WRITING.txt` says (gr/, rev/, then `gcsas.py` and `toglp.py`).
- `ui.js`: the UI language's reader and the checker of the paper's two propositions; `emit.js` writes it.
- `mechanical.js OUT.json`: a description any contract has, complete and visible.
- `uic.js OUT.json NAME.ui cascade/NAME/platform.vglp`: the compiler; it refuses a description with errors or
  not complete.  `platform.js` reads the platform's requests.
- `mkapp.py . NAME dir`: an app directory for a contract's cascade platform, for the walk below.

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
