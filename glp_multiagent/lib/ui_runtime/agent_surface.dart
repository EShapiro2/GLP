/// Generic two-surface agent UI (paper §7.1 "The Application", §7.4 "A Generic
/// UI", Figure fig:gsg). The app is exactly two surfaces, shown as two screens
/// with bottom-tab navigation:
///
///   • Requests — the inbox: ReqId-bearing notifies, each answered Accept/Decline.
///   • Friends  — the state the outbox leaves: a wholly-ground activity rule
///                lands here (`connected` adds a friend, `unfriended` removes
///                one). There is no separate "Activity" screen.
///
/// The "+" on the Friends screen composes outbox requests (offer friendship,
/// introduce). Renders strictly from a [Manifest]; no app-specific constructor
/// name appears here.
library;

import 'package:flutter/material.dart';

import 'manifest.dart';
import 'runtime.dart';
import 'term.dart';

class AgentSurface extends StatefulWidget {
  final String agentId;
  final UiRuntime runtime;
  const AgentSurface({super.key, required this.agentId, required this.runtime});

  @override
  State<AgentSurface> createState() => _AgentSurfaceState();
}

class _AgentSurfaceState extends State<AgentSurface> {
  /// 0 = Friends (the outbox state), 1 = Requests (the inbox).
  int _tab = 0;

  Manifest get _m => widget.runtime.manifest;
  UiRuntime get _r => widget.runtime;

  static const MaterialColor _accent = Colors.orange;

  @override
  Widget build(BuildContext context) {
    final requests = _r.inbox.length;
    final onFriends = _tab == 0;
    return Scaffold(
      backgroundColor: Colors.white,
      appBar: AppBar(
        backgroundColor: _accent,
        foregroundColor: Colors.white,
        elevation: 0,
        title: Text(onFriends ? _m.title : 'Requests',
            style: const TextStyle(fontWeight: FontWeight.bold, fontSize: 16)),
        actions: [
          Padding(
            padding: const EdgeInsets.only(right: 12),
            child: CircleAvatar(
              radius: 13,
              backgroundColor: Colors.white24,
              child: Text(_initial(widget.agentId),
                  style: const TextStyle(
                      color: Colors.white,
                      fontSize: 12,
                      fontWeight: FontWeight.bold)),
            ),
          ),
        ],
      ),
      body: onFriends ? _friendsScreen() : _requestsScreen(),
      floatingActionButton: onFriends
          ? FloatingActionButton(
              backgroundColor: _accent,
              foregroundColor: Colors.white,
              onPressed: () => _composeSheet(context),
              child: const Icon(Icons.add),
            )
          : null,
      bottomNavigationBar: NavigationBar(
        height: 60,
        selectedIndex: _tab,
        onDestinationSelected: (i) => setState(() => _tab = i),
        destinations: [
          const NavigationDestination(
              icon: Icon(Icons.people_outline),
              selectedIcon: Icon(Icons.people),
              label: 'Friends'),
          NavigationDestination(
            icon: _badge(requests, const Icon(Icons.mail_outline)),
            selectedIcon: _badge(requests, const Icon(Icons.mail)),
            label: 'Requests',
          ),
        ],
      ),
    );
  }

  Widget _badge(int count, Widget child) =>
      Badge(isLabelVisible: count > 0, label: Text('$count'), child: child);

  // === Friends screen — the state the outbox leaves =========================

  Widget _friendsScreen() {
    final children = <Widget>[];
    for (final v in _m.state) {
      switch (v.kind) {
        case StateKind.list:
          final items = _r.store.lists[v.key] ?? const [];
          if (items.isEmpty) {
            children.add(_empty('No ${v.label.toLowerCase()} yet'));
          } else {
            children.addAll(items.map((t) => _friendTile(formatTerm(t))));
          }
        case StateKind.value:
          final val = _r.store.values[v.key];
          children.add(ListTile(
            title: Text(v.label),
            trailing: Text(val == null ? '—' : formatTerm(val)),
          ));
        case StateKind.thread:
          final t = _r.store.threads[v.key] ?? const {};
          for (final e in t.entries) {
            children.add(ListTile(
              title: Text(e.key),
              subtitle: Text(e.value.map(formatTerm).join(', ')),
            ));
          }
      }
    }
    if (children.isEmpty) children.add(_empty('Nothing yet'));
    return ListView(
        padding: const EdgeInsets.symmetric(vertical: 4), children: children);
  }

  Widget _friendTile(String name) {
    final display =
        name.isEmpty ? name : name[0].toUpperCase() + name.substring(1);
    return ListTile(
      leading: CircleAvatar(
        backgroundColor: _accent.shade100,
        child: Text(_initial(name),
            style: const TextStyle(
                color: Colors.black87, fontWeight: FontWeight.bold)),
      ),
      title: Text(display, style: const TextStyle(fontWeight: FontWeight.w600)),
    );
  }

  // === Requests screen — the inbox ==========================================

  Widget _requestsScreen() {
    if (_r.inbox.isEmpty) return _empty('No requests');
    return ListView(
      padding: const EdgeInsets.all(8),
      children: _r.inbox.map((c) => _card(context, c)).toList(),
    );
  }

  Widget _card(BuildContext context, InboxCard card) {
    return Card(
      margin: const EdgeInsets.symmetric(vertical: 4, horizontal: 4),
      child: Padding(
        padding: const EdgeInsets.all(12),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Row(
              children: [
                CircleAvatar(
                  radius: 16,
                  backgroundColor: _accent.shade100,
                  child: Text(_titleInitial(card),
                      style: const TextStyle(
                          color: Colors.black87,
                          fontWeight: FontWeight.bold,
                          fontSize: 13)),
                ),
                const SizedBox(width: 10),
                Expanded(
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      Text(renderTemplate(card.desc.title, card.fields),
                          style: const TextStyle(
                              fontWeight: FontWeight.bold, fontSize: 14)),
                      if (card.desc.subtitle != null)
                        Text(renderTemplate(card.desc.subtitle!, card.fields),
                            style: const TextStyle(
                                fontSize: 12, color: Colors.black54)),
                    ],
                  ),
                ),
              ],
            ),
            const SizedBox(height: 10),
            Row(
              children: card.desc.answers.map((a) {
                final primary = a.label.toLowerCase() == 'accept';
                final btn = primary
                    ? ElevatedButton(
                        onPressed: () => _answer(context, card, a),
                        child: Text(a.label))
                    : OutlinedButton(
                        onPressed: () => _answer(context, card, a),
                        child: Text(a.label));
                return Padding(
                    padding: const EdgeInsets.only(right: 8), child: btn);
              }).toList(),
            ),
          ],
        ),
      ),
    );
  }

  void _answer(BuildContext context, InboxCard card, AnswerDesc answer) {
    if (answer.needsPicker) return; // reserved for child-safe pickers
    _r.answerCard(card, answer);
  }

  // === Outbox — compose a request ("+") =====================================

  void _composeSheet(BuildContext context) {
    if (_m.commands.length == 1) {
      _composeCommand(context, _m.commands.first);
      return;
    }
    showModalBottomSheet<void>(
      context: context,
      builder: (ctx) => SafeArea(
        child: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            for (final c in _m.commands)
              ListTile(
                leading: const Icon(Icons.edit_outlined),
                title: Text(c.label),
                onTap: () {
                  Navigator.pop(ctx);
                  _composeCommand(context, c);
                },
              ),
          ],
        ),
      ),
    );
  }

  Future<void> _composeCommand(BuildContext context, CommandDesc cmd) async {
    final controllers = {
      for (final f in cmd.args) f.name: TextEditingController(),
    };
    final ok = await showDialog<bool>(
      context: context,
      builder: (ctx) => AlertDialog(
        title: Text(cmd.label),
        content: Column(
          mainAxisSize: MainAxisSize.min,
          children: cmd.args
              .map((f) => TextField(
                    controller: controllers[f.name],
                    keyboardType: f.type == FieldType.integer
                        ? TextInputType.number
                        : TextInputType.text,
                    decoration: InputDecoration(labelText: f.label),
                  ))
              .toList(),
        ),
        actions: [
          TextButton(
              onPressed: () => Navigator.pop(ctx, false),
              child: const Text('Cancel')),
          ElevatedButton(
              onPressed: () => Navigator.pop(ctx, true),
              child: const Text('Send')),
        ],
      ),
    );
    if (ok != true) return;

    final values = <String, GTerm>{};
    for (final f in cmd.args) {
      final raw = controllers[f.name]!.text.trim();
      if (raw.isEmpty) return;
      values[f.name] = _fieldTerm(f, raw);
    }
    _r.submitCommand(cmd, values);
  }

  GTerm _fieldTerm(FieldDesc f, String raw) {
    switch (f.type) {
      case FieldType.integer:
        return GInt(int.tryParse(raw) ?? 0);
      case FieldType.person:
        return GAtom(raw.toLowerCase());
      case FieldType.text:
        return GAtom(raw);
    }
  }

  // === helpers ==============================================================

  Widget _empty(String text) => Padding(
        padding: const EdgeInsets.symmetric(vertical: 40, horizontal: 16),
        child: Center(
          child: Text(text,
              style: TextStyle(
                  fontSize: 14,
                  fontStyle: FontStyle.italic,
                  color: Colors.grey.shade500)),
        ),
      );

  String _initial(String s) => s.isEmpty ? '?' : s[0].toUpperCase();

  String _titleInitial(InboxCard card) {
    for (final entry in card.fields.entries) {
      final t = entry.value;
      if (t is GAtom && t.name.isNotEmpty) return t.name[0].toUpperCase();
    }
    return '?';
  }
}
