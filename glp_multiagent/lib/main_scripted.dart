/// The Grassroots App's scripted build (GLP/phones/docs/debugging.md, "The
/// scripted build"): the app of main_superapp.dart, its agent, core and the
/// layer unchanged, whose person is a scripted person of
/// `programs/social/graph/core/superapp_plays.glp`, run at the person's
/// channel as under `:boot`, chosen at build time by the Dart define
/// `GLP_PERSON=<mode>`, the persons it knows at the start by `GLP_KNOWN`
/// (scripted_person.dart).  The reporter is on where the build has
/// `GLP_DEBUGLOG` too, as `phones/tools/build.sh <udid> <mode>` builds it.
///
/// A build without `GLP_PERSON`, or with a mode no play runs, shows that on
/// the app's line and starts no agent.
library;

import 'package:flutter/widgets.dart';

import 'main_superapp.dart' show GrassrootsApp;
import 'scripted_person.dart';

void main() {
  WidgetsFlutterBinding.ensureInitialized();
  runApp(GrassrootsApp(
      script: () => ScriptedPerson.parse(scriptedMode, scriptedKnown)));
}
