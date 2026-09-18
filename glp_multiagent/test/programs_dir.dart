/// The `programs/` tree the package's tests read: the checkout's own, never
/// another clone's.
///
/// `flutter test` runs with `glp_multiagent/` as the working directory, in
/// the suite (`test/run_all_tests.sh`, Section Q) and by hand, so the
/// checkout's `programs/` is the working directory's sibling --- a worktree's
/// when the package is a worktree's.  Until 2026-09-18 the tests below named
/// `/Users/udi/Grassroots/GLP/programs` outright, `main`'s clone, so a
/// branch's Dart ran against `main`'s `.glp`: a `.glp` change on the branch
/// was invisible to them, and their green or red said nothing about the
/// branch (IGLP, 2026-09-18).
///
/// There is no fall-back to that clone.  None of these tests can run outside
/// a checkout, so an absent `programs/` is an error naming the directory
/// looked for, never a silent read of another tree.
library;

import 'dart:io';

/// Absolute path of the checkout's `programs/`, resolved from the working
/// directory.  Throws a [StateError] naming the path when it is not there.
String programsDir() {
  final dir = Directory('${Directory.current.parent.path}/programs');
  if (!dir.existsSync()) {
    throw StateError('programs/ not found at ${dir.path}: run flutter test '
        'from glp_multiagent/ inside a GLP checkout');
  }
  return dir.path;
}
