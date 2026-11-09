# Testing the Grassroots App on your own phones

Phones #1 Cowork, 2026-11-09, Udi approving (07:01--07:03 UTC).  For Nimrod and Dan, and anyone who joins them: how you build the app, run the plays, report what fails, and send fixes.

## What you read first

1. `GLP/CLAUDE.md`, the rules every change to the repository follows.
2. The two papers the app is built from: GSA, the Grassroots Super-App and App Store, and GNA, the networking layer.  The paper is the specification: what the code does is never evidence of what it should do.
3. `phones/docs/debugging.md`: the log service, the reporter in the app, and the plays.
4. `phones/plays/`: the plays, each a script for the persons and the events each phone's log must show.
5. `phones/docs/STATE.md`: what has run on phones so far and what failed.

## Building on your own Mac

Each of you builds and signs the app on your own Mac with your own Apple team, until the project's paid account lets us distribute it.

1. Clone `https://github.com/EShapiro2/GLP`; Flutter, Xcode, `ideviceinstaller` (Homebrew, `libimobiledevice`) and Python 3 on the path.
2. Set your team and a bundle identifier of your own in `glp_multiagent/ios/Runner.xcodeproj`, since the app's identifier is registered to Udi's team.  Do not commit this change.
3. Put the debugging log's password, which Udi gives you, in `$GRASSROOTS/.debuglog-password`, mode 600; `GRASSROOTS` is any directory of yours, and `build.sh` writes its builds under `$GRASSROOTS/tmp/phones`.
4. Connect each phone once and run `flutter run -d <udid> -t lib/main_superapp.dart` from `glp_multiagent`, which registers it with your team.
5. `GRASSROOTS=<dir> phones/tools/build.sh <udid>` builds the app with the reporter on and installs it; `phones/tools/devices.sh` lists your phones.  With a mode, `build.sh <udid> <mode>` builds the scripted app, whose person runs a play by itself.

## Running a play

Run a play as its file says: the network, the persons' steps in order, and what each phone must show.  Every event of your phones goes to the log service at `https://peoplesweb.org/log`, under each phone's key and the launch's run; `phones/tools/follow.sh <phone> <run>` follows a run as it arrives.  A play is judged by its expectations against the logs; it is never changed to pass.

To be found by people who do not know you, list yourself with the directory at `https://peoplesweb.org/directory` from the app, once the app offers List.

## Reporting what fails

Each fault is a GitHub issue on `EShapiro2/GLP`, labelled `phones`, one issue a fault, naming the play and its step, the phones and their keys, the log runs, the build (GLP's commit) and what the log shows against what the play expects.  Phones reads the issues, answers there, and sends each to Code where the code is wrong and to the paper's owner where the paper is.  A fault in GLP, its engine or the networking layer is a blocker: say so in the issue.

## Sending a fix

1. One pull request a fix, from a branch off `main`, closing its issue, and naming the play and the log run it fixes.
2. A fix that brings the code to the paper needs no approval; a change that goes beyond the paper, or changes the language or its engine, goes first to the paper's owner and to Udi, through Phones in the issue.
3. Code reviews each pull request, runs the suite on it and merges it to `main` as soon as the suite passes; a blocker the same hour.  Nobody else merges to `main`.
4. Networking fixes go to `GLP/networking`: it was imported from `grassroots-networking`, and GNA's paper is its specification (GNA, Implementation Notes, "The Code").
5. Every GLP procedure is declared, with no `_` or `_?` in a type (`docs/typed-glp-manual.md`, section 18); no workaround, no test marked expected to fail.
