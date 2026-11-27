/// The Grassroots App's screens on a phone: home, as the super-app's program draws it, and each
/// mini-app the person installed, as the mini-app draws it on its own person channel, which the
/// super-app shows under its name (programs/social/graph/core/self.glp, shown_superapp/7,
/// app_screens/1; [SuperAppHost.apps]).  The person moves between them by the row on top, which
/// says where she is; opening a mini-app whose screen is still empty begins it, with her alone
/// (activate).  Installing a mini-app, inviting a friend to one and answering a friend's
/// invitation are home's own request and cards (home.vglp: install_carried, invite_card/4,
/// respond_invite/4), drawn with the rest of home.
library;

import 'package:flutter/material.dart';

import 'superapp_host.dart';
import 'ui_runtime/agent_surface.dart';

/// The mini-apps the app carries, by the name home installs them under (home.vglp, Carried),
/// and the name the person reads.
const Map<String, String> carriedTitles = {'friendship': 'Friendship', 'groups': 'Groups', 'coins': 'Coins'};

/// How a mini-app's screen is drawn: its surface, which a host may wrap, as with a description of
/// its screens; by default the surface as it is.
typedef AppScreenBuilder = Widget Function(String app, String title, Widget surface);

class SuperAppScreens extends StatefulWidget {
  final SuperAppHost host;

  /// Home's screen, as the super-app's program draws it.
  final Widget home;

  /// The mini-app's screen, its surface wrapped.
  final AppScreenBuilder? appScreen;
  const SuperAppScreens({super.key, required this.host, required this.home, this.appScreen});

  @override
  State<SuperAppScreens> createState() => _SuperAppScreensState();
}

class _SuperAppScreensState extends State<SuperAppScreens> {
  /// Where the person is: null home, else a mini-app's name.
  String? _at;

  /// The mini-apps the person opened here, begun once each.
  final Set<String> _begun = {};

  SuperAppHost get host => widget.host;

  @override
  void initState() {
    super.initState();
    host.onAppsChange = () {
      if (mounted) setState(() {});
    };
  }

  String _title(String app) => carriedTitles[app] ?? app;

  void _open(String app) {
    setState(() => _at = app);
    final ui = host.apps[app];
    if (ui != null && ui.bridge.constructs.isEmpty && _begun.add(app)) host.activate(app);
  }

  @override
  Widget build(BuildContext context) {
    final theme = Theme.of(context);
    final tabs = <(String?, String)>[(null, 'Home'), for (final n in host.apps.keys) (n, _title(n))];
    final at = _at, ui = at == null ? null : host.apps[at];
    return Column(children: [
      Material(
        color: theme.colorScheme.surface,
        child: SingleChildScrollView(
          key: const ValueKey('superapp:where'),
          scrollDirection: Axis.horizontal,
          padding: const EdgeInsets.fromLTRB(8, 4, 8, 4),
          child: Row(children: [
            for (final (a, label) in tabs)
              Padding(
                padding: const EdgeInsets.only(right: 6),
                child: ChoiceChip(
                  key: ValueKey('superapp:tab:${a ?? 'home'}'),
                  label: Text(label),
                  selected: _at == a,
                  onSelected: (_) => a == null ? setState(() => _at = null) : _open(a),
                ),
              ),
          ]),
        ),
      ),
      const Divider(height: 1),
      Expanded(
        child: at == null || ui == null
            ? widget.home
            : ui.bridge.constructs.isEmpty
                ? Center(child: Text('Opening ${_title(at)}…', style: const TextStyle(color: Colors.grey)))
                : (widget.appScreen ?? (a, t, surface) => surface)(at, _title(at), AgentSurface(agentId: host.agentId, runtime: ui)),
      ),
    ]);
  }
}
