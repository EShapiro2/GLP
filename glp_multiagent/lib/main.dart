/// The phone app's default: the Grassroots Super-App,
/// `programs/social/graph/core` (GSG, Section 7), whose mini-apps are each
/// platform's own program run as a mini-app.
///
/// The one entry is `main_superapp.dart`; this file names it as the default
/// build target and holds no copy of it.
library;

import 'main_superapp.dart' as superapp;

void main() => superapp.main();
