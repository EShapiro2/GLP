# UI structures and UI styles (GCE)

An app's UI structure, its screens, what each shows and where each act is requested from, in the language of
the paper The Grassroots UI Layer, in the contract's own words, and checked against the contract: complete (every act a person may take is offered) and visible (every fact
shown).

- `Friendship.ui`: Friendship's UI structure, person first: everyone Alice knows on the start screen, and a screen for each.
- `Friendship.style`: its UI style, how it is drawn, one sentence a line, by the names the UI structure uses: a typed value
  LONG or a SLIDER, a list A GRID, an action IN THE MENU or PRIMARY, the colors of the whole app.  Its checker
  refuses a UI style that loses an option the contract allows, such as a slider narrower than the values allowed.
- `Groups.ui`, `Groups.style` and `Groups.ui.json`: Groups (GSN's app, `programs/apps/gsn`), conversations first:
  the person's groups and contacts on the start screen, a chat with each contact, a screen for each group.
- `Friendship.ui.json`: the UI structure and the UI style, compiled with the contract into what the app reads: each
  act by its schema, each place by its number in the act's request, the proposer's place the platform's.  Edit
  the `.ui` or the `.style` and print it again:
  `node tools/uic.js CONTRACT.json Friendship.ui cascade/Friendship/platform.vglp --style Friendship.style`.

## The tools (`tools/`, Node and Python 3, no Dart)

- `build.py CALC NAME OUT.json`: the contract as the tools read it, from a scratch copy of `calculus/` made as
  `calculus/tools/SAS-WRITING.txt` says (gr/, rev/, then `gcsas.py` and `toglp.py`).
- `ui.js`: the UI language's reader and the checker of the paper's two propositions; `emit.js` writes it.
- `mechanical.js OUT.json`: a UI structure any contract has, complete and visible.
- `uic.js CONTRACT.json NAME.ui cascade/NAME/platform.vglp [--style NAME.style]`: the compiler; it refuses a
  UI structure with errors or not complete, and a UI style that loses an option.  `platform.js` reads the platform's requests.
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
