# UI structures and UI styles (GCE)

An app's UI structure, its screens, what each shows and where each act is requested from, in the language of
the paper The Grassroots UI Layer, in the contract's own words, and checked against the contract: complete (every act a person may take is offered) and visible (every fact
shown).

- `Friendship.ui`: Friendship's UI structure, home first: everyone Alice knows on the start screen, befriending there in
  one tap with Make friends (a chain: Acquaint AND Befriend), and a screen for each person.
- `Friendship.style`: its UI style, how it is drawn, one sentence a line, by the names the UI structure uses: a typed value
  LONG or a SLIDER, a list A GRID and NEWEST FIRST or ALPHABETICAL, an action IN THE MENU or PRIMARY, a value
  DRAWN BY a drawing program or PICKED ON one (`tools/pictures.js`: chessboard, tens), the colors of the whole app.  Its checker
  refuses a UI style that loses an option the contract allows, such as a slider narrower than the values allowed.
- `Groups.ui`, `Groups.style` and `Groups.ui.json`: Groups (GSN's app, `programs/apps/gsn`), conversations first:
  the person's groups and contacts on the start screen, a chat with each contact, a screen for each group.
- `App.ui`, `App.style` and `App.ui.json`: the Grassroots App's own screens, home, from its contract App
  (`programs/social/graph/App.txt`, platform `cascade/App/platform.vglp`): apps, people and stores on the start
  screen, a screen for each, Nearby and links; the rarer acts on More, a landmark: a second screen about the
  person herself, reached from every screen.
- `Friendship.ui.json`: the UI structure and the UI style, compiled with the contract into what the app reads: each
  act by its schema, each place by its number in the act's request, the proposer's place the platform's.  Edit
  the `.ui` or the `.style` and print it again:
  `node tools/uic.js CONTRACT.json Friendship.ui cascade/Friendship/platform.vglp --style Friendship.style`.

## Important acts, the way to them, and THEN IT

- `X IS IMPORTANT.` (UI style; X a button, alone or `X ON Screen`): where X is placed and not offered now, the
  screen shows in its place what is in its way, read off the contract (the first fact it requires that does not
  hold, or that it forbids and holds, with the screen's names bound), and the way to it: the person's own acts
  whose effects bring that fact, searched backward three deep, the first one she can take now (on this screen,
  the start screen or a landmark) as a button, and the rest named ("then Install").  A fact no act of hers
  brings comes when another person takes the act that does.  In the `.ui.json`: `"important": true` on the
  action or chain, and at the top `effects` (each act's button and the predicates it adds to and ends for its
  proposer) and `addedBy` (each predicate, the acts that add it, `mine` when to the proposer), from which the
  app searches.  The checker refuses an IMPORTANT that names no button and warns when a fact an important act
  requires is added by no act.
- `..., THEN IT` (UI structure): once the act is done, the screen that a list on the start screen or a landmark
  opens about the fact the act added; the checker requires such a list.  In the `.ui.json`:
  `"then": "IT", "thenIt": {"screen", "pred", "of": "fact" | "value", "from"}`; the app opens `screen` about the
  newest fact `pred` of the person (`of: value`: about its value, a list `FROM pred`), back to `from`.
- `Name IS CALLED "label".` for a typed name labels its field: `places[k].look.label`.

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
