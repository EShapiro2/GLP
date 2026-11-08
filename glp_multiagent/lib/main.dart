/// The phone app's default: the Grassroots App,
/// `programs/social/graph/core` at superapp/4, over the real network (GSA
/// paper, Section 2 and Appendix "The Operating System").
///
/// The entry is `main_superapp.dart`; this file names it as the default
/// build target and holds no copy of it.  The demonstration with scripted
/// peers is `main_superapp_ui.dart`.
library;

import 'main_superapp.dart' as superapp;

void main() => superapp.main();
