/// The Grassroots App's screens on a phone: home, as the super-app's program draws it, and each
/// mini-app the person installed, as the mini-app draws it on its own person channel, which the
/// super-app shows under its name (programs/social/graph/core/self.glp, shown_superapp/7,
/// app_screens/1; [SuperAppHost.apps]).  The person moves between them by the row on top, which
/// says where she is.  Apps lists the mini-apps the app carries, each installed by one tap (the
/// super-app's install_bundled: the artefact read beside the program, its compiler trusted, and
/// installed, as each app's boot installs it); an installed mini-app is opened by a tap, which
/// activates it, and on its screen the person invites a friend to it.  A friend's invitation she
/// answers on home's own card (home.vglp, respond_invite/4).
library;

import 'package:flutter/material.dart';

import 'superapp_host.dart';
import 'ui_runtime/agent_surface.dart';
import 'ui_runtime/term.dart';

/// A mini-app the app carries: its name as the super-app installs it, its artefact beside the
/// program (tool/sync_glp_assets.sh), and the name the person reads.
typedef BundledApp = ({String name, String artefact, String title});

/// The mini-apps the app carries, as tool/sync_glp_assets.sh writes their artefacts.
const List<BundledApp> bundledApps = [
  (name: 'gsg', artefact: 'gsg.glpw', title: 'Friendship'),
  (name: 'gsn', artefact: 'gsn.glpw', title: 'Groups'),
  (name: 'currency', artefact: 'currency.glpw', title: 'Coins'),
];

/// How a mini-app's screen is drawn: its surface, which a host may wrap, as with a description of
/// its screens; by default the surface as it is.
typedef AppScreenBuilder = Widget Function(BundledApp app, Widget surface);

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
  /// Where the person is: null home, '+' the apps the app carries, else a mini-app's name.
  String? _at;

  SuperAppHost get host => widget.host;

  @override
  void initState() {
    super.initState();
    host.onAppsChange = () {
      if (mounted) setState(() {});
    };
  }

  /// The person's friends, as the super-app told her: `connected(K)` among home's notices.
  List<String> get friends {
    final v = host.ui.bridge.construct(0)?.view;
    final out = <String>[];
    if (v is GList) {
      for (final m in v.items) {
        final (_, args) = ctorArgs(m);
        final content = args.length == 3 ? args[2] : m;
        if (content is GStruct && content.functor == 'connected' && content.args.single is GAtom) {
          final k = (content.args.single as GAtom).name;
          if (!out.contains(k)) out.add(k);
        }
      }
    }
    return out;
  }

  BundledApp _app(String name) =>
      bundledApps.firstWhere((a) => a.name == name, orElse: () => (name: name, artefact: '$name.glpw', title: name));

  Future<void> _install(BundledApp a) async {
    await host.appAct('install_bundled', a.name, a.artefact);
    if (mounted) setState(() {});
  }

  Future<void> _open(String name) async {
    setState(() => _at = name);
    if (host.apps[name] == null) await host.appAct('activate', name);
  }

  String _short(String key) => key.length > 8 ? '${key.substring(0, 8)}…' : key;

  @override
  Widget build(BuildContext context) {
    final theme = Theme.of(context);
    final tabs = <(String?, String)>[
      (null, 'Home'),
      for (final n in {...host.installed, ...host.apps.keys}) (n, _app(n).title),
      ('+', 'Apps'),
    ];
    return Column(children: [
      Material(
        color: theme.colorScheme.surface,
        child: SingleChildScrollView(
          key: const ValueKey('superapp:where'),
          scrollDirection: Axis.horizontal,
          padding: const EdgeInsets.fromLTRB(8, 4, 8, 4),
          child: Row(children: [
            for (final (at, label) in tabs)
              Padding(
                padding: const EdgeInsets.only(right: 6),
                child: ChoiceChip(
                  key: ValueKey('superapp:tab:${at ?? 'home'}'),
                  label: Text(label),
                  selected: _at == at,
                  onSelected: (_) => at == null || at == '+' ? setState(() => _at = at) : _open(at),
                ),
              ),
          ]),
        ),
      ),
      const Divider(height: 1),
      Expanded(child: _body(theme)),
    ]);
  }

  Widget _body(ThemeData theme) {
    final at = _at;
    if (at == null) return widget.home;
    if (at == '+') {
      return ListView(key: const ValueKey('superapp:apps'), padding: const EdgeInsets.all(16), children: [
        Text('Apps on this phone', style: theme.textTheme.titleMedium),
        const SizedBox(height: 8),
        for (final a in bundledApps)
          ListTile(
            key: ValueKey('superapp:app:${a.name}'),
            title: Text(a.title),
            trailing: host.installed.contains(a.name)
                ? TextButton(onPressed: () => _open(a.name), child: const Text('Open'))
                : FilledButton.tonal(onPressed: () => _install(a), child: const Text('Install')),
          ),
      ]);
    }
    final a = _app(at), ui = host.apps[at];
    final fs = friends;
    return Column(children: [
      Padding(
        padding: const EdgeInsets.fromLTRB(12, 6, 12, 0),
        child: Wrap(spacing: 8, runSpacing: 4, crossAxisAlignment: WrapCrossAlignment.center, children: [
          if (fs.isNotEmpty)
            PopupMenuButton<String>(
              key: ValueKey('superapp:invite:$at'),
              onSelected: (f) => host.appAct('invite', at, f),
              itemBuilder: (_) => [for (final f in fs) PopupMenuItem(value: f, child: Text('Invite ${_short(f)}'))],
              child: const Chip(avatar: Icon(Icons.person_add_alt, size: 18), label: Text('Invite a friend')),
            ),
        ]),
      ),
      Expanded(
        child: ui == null
            ? Center(child: Text('Opening ${a.title}…', style: const TextStyle(color: Colors.grey)))
            : (widget.appScreen ?? (_, s) => s)(a, AgentSurface(agentId: host.agentId, runtime: ui)),
      ),
    ]);
  }
}
