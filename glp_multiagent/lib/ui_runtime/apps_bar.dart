/// The mini-apps installed on the super-app, on its screen: a row with one
/// chip for each, marked with how many of its questions wait for the person
/// ([AppScreens.waiting]), and a page for each, where its own screen is drawn
/// (EShapiro2/GLP#181).
library;

import 'package:flutter/material.dart';

import 'agent_surface.dart';
import 'app_screens.dart';
import 'look.dart';

/// One chip for each installed mini-app, marked while its questions wait.
class AppsBar extends StatelessWidget {
  final AppScreens screens;
  final void Function(String app) open;
  const AppsBar({super.key, required this.screens, required this.open});

  @override
  Widget build(BuildContext context) {
    return ListenableBuilder(
      listenable: screens,
      builder: (context, _) => SizedBox(
        height: 48,
        child: ListView(
          scrollDirection: Axis.horizontal,
          padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 6),
          children: [
            for (final app in screens.apps)
              Padding(
                padding: const EdgeInsets.only(right: 8),
                child: Badge(
                  isLabelVisible: screens.waiting(app) > 0,
                  label: Text('${screens.waiting(app)}'),
                  child: ActionChip(
                    key: ValueKey('app:$app'),
                    label: Text(app),
                    onPressed: () => open(app),
                  ),
                ),
              ),
          ],
        ),
      ),
    );
  }
}

/// The page of one installed mini-app: its own screen, as its program draws it.
class AppPage extends StatelessWidget {
  final String app;
  final AppScreens screens;
  final ContractLook? look;

  /// Wraps the mini-app's surface, as a description of its screens does
  /// (described.dart); null, and the surface is drawn as it is.
  final Widget Function(String app, Widget surface)? wrap;

  const AppPage({super.key, required this.app, required this.screens, this.look, this.wrap});

  @override
  Widget build(BuildContext context) {
    return ListenableBuilder(
      listenable: screens,
      builder: (context, _) {
        final ui = screens.screen(app);
        Widget body = ui == null
            ? const SizedBox.shrink()
            : AgentSurface(agentId: app, runtime: ui, icon: look?.icon);
        if (wrap != null) body = wrap!(app, body);
        if (look != null) body = Theme(data: look!.theme(), child: body);
        return Scaffold(
          appBar: AppBar(title: Text(app)),
          body: SafeArea(child: body),
        );
      },
    );
  }
}
