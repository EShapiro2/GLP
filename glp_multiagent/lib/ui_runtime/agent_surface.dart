/// GrassApp — one app, one panel per platform (paper §7, Figure fig:grassapp).
///
/// The bottom bar is the panels — Friends (social graph), Coins, Chats (social
/// network) — each reached by an icon badged with that panel's pending alerts.
/// The app bar reads the active panel's name. Each panel renders the two
/// constructs and the screen (paper §6): compose forms (its "+", the
/// Request-shaped clauses) and inbox cards (the Respond-shaped pending
/// reductions), surfaced as WhatsApp-style per-item alerts — a card badges the
/// row it pins to (a person, a friend, a group) and is answered by tapping
/// that row, which opens a sheet whose buttons are the sibling clauses (tap
/// grants, dismiss cancels — a confirmed gesture, not inline buttons).
///
/// One mediator feeds one runtime; the runtime tags each card with its owning
/// panel, so this surface routes alerts by panel and row. Renders strictly from
/// a [Manifest]; no app-specific constructor name appears here.
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
  /// The active panel (index into the manifest's panels).
  int _panel = 0;

  /// Drill-down within the active panel: an open chat peer or wallet person
  /// (null = the panel's list).
  String? _openItem;

  final TextEditingController _msg = TextEditingController();

  @override
  void initState() {
    super.initState();
    widget.runtime.onNotice = (message) {
      if (!mounted) return;
      ScaffoldMessenger.of(context).showSnackBar(SnackBar(
        content: Text(message),
        duration: const Duration(seconds: 2),
        behavior: SnackBarBehavior.floating,
      ));
    };
  }

  Manifest get _m => widget.runtime.manifest;
  UiRuntime get _r => widget.runtime;

  static const MaterialColor _accent = Colors.green;

  // === Per-panel card routing ===============================================

  /// The pending cards belonging to [panel], keyed by the row they alert on.
  Map<String, InboxCard> _pendingByItem(Panel panel) {
    final m = <String, InboxCard>{};
    for (final c in _r.inbox) {
      if (c.panel.id == panel.id) m[c.itemKey] = c;
    }
    return m;
  }

  int _panelAlerts(Panel panel) =>
      _r.inbox.where((c) => c.panel.id == panel.id).length;

  IconData _panelIcon(Panel p, {required bool selected}) {
    if (p.wallet != null) {
      return selected ? Icons.monetization_on : Icons.monetization_on_outlined;
    }
    if (p.chat != null) {
      return selected ? Icons.chat_bubble : Icons.chat_bubble_outline;
    }
    if (p.groups != null) {
      return selected ? Icons.groups : Icons.groups_outlined;
    }
    return selected ? Icons.people : Icons.people_outline;
  }

  @override
  Widget build(BuildContext context) {
    final panels = _m.panels;
    if (_panel >= panels.length) _panel = panels.length - 1;
    final active = panels[_panel];

    // Drill-down within the active panel: a wallet person or a chat peer.
    if (_openItem != null) {
      if (active.chat != null) return _conversation(active.chat!, _openItem!);
      if (active.wallet != null) return _walletDetail(active.wallet!, _openItem!);
      if (active.groups != null) {
        return _groupConversation(active.groups!, _openItem!);
      }
    }

    final showFab = active.commands.isNotEmpty;
    return Scaffold(
      backgroundColor: Colors.white,
      appBar: AppBar(
        backgroundColor: _accent,
        foregroundColor: Colors.white,
        elevation: 0,
        title: Text(active.name,
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
      body: _panelBody(active),
      floatingActionButton: showFab
          ? FloatingActionButton(
              backgroundColor: _accent,
              foregroundColor: Colors.white,
              onPressed: () => _composeSheet(context, active.commands),
              child: const Icon(Icons.add),
            )
          : null,
      bottomNavigationBar: NavigationBar(
        height: 60,
        selectedIndex: _panel,
        onDestinationSelected: (i) => setState(() {
          _panel = i;
          _openItem = null;
        }),
        destinations: [
          for (final p in panels)
            NavigationDestination(
              icon: _alertBadge(
                  _panelAlerts(p), Icon(_panelIcon(p, selected: false))),
              selectedIcon: _alertBadge(
                  _panelAlerts(p), Icon(_panelIcon(p, selected: true))),
              label: p.name,
            ),
        ],
      ),
    );
  }

  Widget _alertBadge(int count, Widget child) =>
      Badge(isLabelVisible: count > 0, label: Text('$count'), child: child);

  Widget _panelBody(Panel p) {
    if (p.wallet != null) return _walletPanel(p);
    if (p.chat != null) return _chatPanel(p);
    if (p.groups != null) return _groupsPanel(p);
    return _friendsPanel(p);
  }

  // === The per-item alert: tap a row → accept/decline (confirmed gesture) ====

  void _openCard(BuildContext context, InboxCard card) {
    showModalBottomSheet<void>(
      context: context,
      builder: (ctx) => SafeArea(
        child: Padding(
          padding: const EdgeInsets.all(20),
          child: Column(
            mainAxisSize: MainAxisSize.min,
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text(renderTemplate(card.desc.title, card.fields),
                  style: const TextStyle(
                      fontWeight: FontWeight.bold, fontSize: 16)),
              if (card.desc.subtitle != null) ...[
                const SizedBox(height: 6),
                Text(renderTemplate(card.desc.subtitle!, card.fields),
                    style: const TextStyle(fontSize: 14, color: Colors.black54)),
              ],
              const SizedBox(height: 18),
              Row(
                mainAxisAlignment: MainAxisAlignment.end,
                children: card.desc.answers.map((a) {
                  final primary = a.label.toLowerCase() == 'accept';
                  final onPressed = () {
                    Navigator.pop(ctx);
                    _answer(card, a);
                  };
                  final btn = primary
                      ? ElevatedButton(
                          onPressed: onPressed, child: Text(a.label))
                      : OutlinedButton(
                          onPressed: onPressed, child: Text(a.label));
                  return Padding(
                      padding: const EdgeInsets.only(left: 8), child: btn);
                }).toList(),
              ),
            ],
          ),
        ),
      ),
    );
  }

  void _answer(InboxCard card, AnswerDesc answer) {
    if (answer.needsPicker) return; // reserved for child-safe pickers
    _r.answerCard(card, answer);
  }

  /// A row's trailing alert affordance: a count badge over a notification dot.
  Widget _rowAlert() => const Icon(Icons.notifications_active, color: _accent);

  // === Friends panel — the social graph =====================================

  Widget _friendsPanel(Panel p) {
    final pending = _pendingByItem(p);
    final friends =
        (_r.store.lists[p.friends!.listKey] ?? const []).map(formatTerm);
    // Established friends, then anyone who has offered but isn't a friend yet
    // (first contact is gated here, so the offer shows as an alerting row).
    final rows = <String>{...friends, ...pending.keys};
    if (rows.isEmpty) return _empty('No friends yet');
    return ListView(
      padding: const EdgeInsets.symmetric(vertical: 4),
      children: [
        for (final person in rows)
          _personTile(person, pending[person], subtitle: null),
      ],
    );
  }

  // === Coins panel — the wallet, organised by friend ========================

  Widget _walletPanel(Panel p) {
    final w = p.wallet!;
    final pending = _pendingByItem(p);
    final people = <String>{w.selfKey};
    if (w.friendsList.isNotEmpty) {
      people.addAll((_r.store.lists[w.friendsList] ?? const []).map(formatTerm));
    }
    people.addAll((_r.store.holdings[w.storeKey] ?? const {}).keys);
    people.addAll(pending.keys);
    return ListView(
      padding: const EdgeInsets.symmetric(vertical: 2),
      children: [
        for (final person in people)
          _personTile(person, pending[person],
              titleOverride: person == w.selfKey ? 'You' : null,
              subtitle: _holdingsSummary(w, person),
              onOpen: () => setState(() => _openItem = person)),
      ],
    );
  }

  // === Chats panel — the social network =====================================

  Widget _chatPanel(Panel p) {
    final pending = _pendingByItem(p);
    final convs = _r.store.threads[p.chat!.threadKey] ?? const {};
    final rows = <String>{...convs.keys, ...pending.keys};
    if (rows.isEmpty) return _empty('No chats yet');
    return ListView(
      padding: const EdgeInsets.symmetric(vertical: 2),
      children: [
        for (final peer in rows)
          _personTile(peer, pending[peer],
              subtitle: convs[peer] == null ? null : _lastText(convs[peer]!),
              onOpen: convs[peer] == null
                  ? null
                  : () => setState(() => _openItem = peer)),
      ],
    );
  }

  // === Shared row: a person/friend/peer, badged when it has a pending card ===

  /// One list row. When [card] is non-null the row carries a per-item alert and
  /// tapping it opens the accept/decline sheet; otherwise tapping runs [onOpen]
  /// (drill into holdings or a conversation), if any.
  Widget _personTile(String name, InboxCard? card,
      {String? titleOverride, String? subtitle, VoidCallback? onOpen}) {
    final alerting = card != null;
    return ListTile(
      leading: CircleAvatar(
        backgroundColor: _accent.shade100,
        child: Text(_initial(name),
            style: const TextStyle(
                color: Colors.black87, fontWeight: FontWeight.bold)),
      ),
      title: Text(titleOverride ?? _cap(name),
          style: const TextStyle(fontWeight: FontWeight.w600)),
      subtitle: alerting
          ? Text(renderTemplate(card.desc.title, card.fields),
              maxLines: 1,
              overflow: TextOverflow.ellipsis,
              style: const TextStyle(color: _accent, fontWeight: FontWeight.w600))
          : (subtitle == null
              ? null
              : Text(subtitle, maxLines: 1, overflow: TextOverflow.ellipsis)),
      trailing: alerting
          ? _rowAlert()
          : (onOpen == null
              ? null
              : const Icon(Icons.chevron_right, color: Colors.grey)),
      onTap: alerting ? () => _openCard(context, card) : onOpen,
    );
  }

  // === Wallet drill-down: a person's coins + the actions against them ========

  Widget _walletDetail(WalletView w, String person) {
    final isSelf = person == w.selfKey;
    final holds = _r.store.holdings[w.storeKey]?[person] ?? const {};
    final coins = holds.keys.toList();
    final actions = isSelf ? w.selfActions : w.friendActions;
    return Scaffold(
      backgroundColor: const Color(0xFFF3EFEA),
      appBar: AppBar(
        backgroundColor: _accent,
        foregroundColor: Colors.white,
        elevation: 0,
        leading: BackButton(onPressed: () => setState(() => _openItem = null)),
        titleSpacing: 0,
        title: Text(isSelf ? 'Your wallet' : "${_cap(person)}'s coins",
            style: const TextStyle(fontWeight: FontWeight.bold, fontSize: 16)),
      ),
      body: Column(
        children: [
          Expanded(
            child: coins.isEmpty
                ? _empty('No coins held')
                : ListView(
                    padding: const EdgeInsets.symmetric(vertical: 4),
                    children: [
                      for (final c in coins)
                        ListTile(
                          leading: const Icon(Icons.toll, color: _accent),
                          title: Text(_coinLabel(c, w),
                              style:
                                  const TextStyle(fontWeight: FontWeight.w600)),
                          trailing: Text(formatTerm(holds[c]!),
                              style: const TextStyle(
                                  fontSize: 16, fontWeight: FontWeight.bold)),
                        ),
                    ],
                  ),
          ),
          if (actions.isNotEmpty)
            Container(
              width: double.infinity,
              color: Colors.white,
              padding: const EdgeInsets.fromLTRB(12, 10, 12, 14),
              child: Wrap(
                spacing: 8,
                runSpacing: 8,
                alignment: WrapAlignment.center,
                children: [
                  for (final a in actions)
                    ElevatedButton(
                      style: ElevatedButton.styleFrom(
                          backgroundColor: _accent,
                          foregroundColor: Colors.white),
                      onPressed: () => _composeCommand(context, a,
                          prefill: isSelf
                              ? const {}
                              : {w.friendField: GAtom(person)}),
                      child: Text(a.label),
                    ),
                ],
              ),
            ),
        ],
      ),
    );
  }

  String _holdingsSummary(WalletView w, String person) {
    final holds = _r.store.holdings[w.storeKey]?[person] ?? const {};
    if (holds.isEmpty) return 'No coins';
    return holds.entries
        .map((e) => '${formatTerm(e.value)} ${_coinShort(e.key, w)}')
        .join(', ');
  }

  String _coinLabel(String coin, WalletView w) =>
      coin == w.selfKey ? 'Your coins' : "${_cap(coin)}'s coins";

  String _coinShort(String coin, WalletView w) =>
      coin == w.selfKey ? 'yours' : "${_cap(coin)}'s";

  // === Chat drill-down: bubbles + input =====================================

  Widget _conversation(ChatView chat, String peer) {
    final msgs = _r.store.threads[chat.threadKey]?[peer] ?? const [];
    return Scaffold(
      backgroundColor: const Color(0xFFF3EFEA),
      appBar: AppBar(
        backgroundColor: _accent,
        foregroundColor: Colors.white,
        elevation: 0,
        leading: BackButton(onPressed: () => setState(() => _openItem = null)),
        titleSpacing: 0,
        title: Text(_cap(peer),
            style: const TextStyle(fontWeight: FontWeight.bold, fontSize: 16)),
      ),
      body: Column(
        children: [
          Expanded(
            child: ListView(
              padding: const EdgeInsets.all(10),
              children: msgs.map(_bubble).toList(),
            ),
          ),
          _inputBar(chat, peer),
        ],
      ),
    );
  }

  Widget _bubble(GTerm m) {
    // Stored as out(text[,tick]) / in(text). Chat texts are plain atoms with
    // `_` for spaces (see chatAtom); render them as prose.
    final s = m is GStruct ? m : GStruct('in', [m]);
    final outgoing = s.functor == 'out';
    final text =
        s.args.isNotEmpty ? formatTerm(s.args[0]).replaceAll('_', ' ') : '';
    final tick = outgoing && s.args.length > 1 ? formatTerm(s.args[1]) : null;
    return Align(
      alignment: outgoing ? Alignment.centerRight : Alignment.centerLeft,
      child: Container(
        margin: const EdgeInsets.symmetric(vertical: 3),
        padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 8),
        constraints: const BoxConstraints(maxWidth: 250),
        decoration: BoxDecoration(
          color: outgoing ? const Color(0xFFEAF3DE) : Colors.white,
          borderRadius: BorderRadius.circular(10),
        ),
        child: Row(
          mainAxisSize: MainAxisSize.min,
          crossAxisAlignment: CrossAxisAlignment.end,
          children: [
            Flexible(child: Text(text, style: const TextStyle(fontSize: 14))),
            if (tick != null) ...[
              const SizedBox(width: 6),
              Icon(
                tick == 'delivered' ? Icons.done_all : Icons.done,
                size: 15,
                color: tick == 'delivered' ? Colors.blue : Colors.grey,
              ),
            ],
          ],
        ),
      ),
    );
  }

  Widget _inputBar(ChatView chat, String peer) {
    void send() {
      final t = _msg.text.trim();
      if (t.isEmpty) return;
      _msg.clear();
      _r.sendChat(chat, peer, t);
    }

    return Container(
      padding: const EdgeInsets.fromLTRB(8, 6, 8, 8),
      color: Colors.white,
      child: Row(
        children: [
          const Icon(Icons.add, color: Colors.grey),
          const SizedBox(width: 6),
          Expanded(
            child: TextField(
              controller: _msg,
              onSubmitted: (_) => send(),
              decoration: InputDecoration(
                hintText: 'Message',
                isDense: true,
                contentPadding:
                    const EdgeInsets.symmetric(horizontal: 14, vertical: 10),
                border: OutlineInputBorder(
                    borderRadius: BorderRadius.circular(20),
                    borderSide: BorderSide(color: Colors.grey.shade300)),
              ),
            ),
          ),
          IconButton(
              onPressed: send, icon: const Icon(Icons.send, color: _accent)),
        ],
      ),
    );
  }

  String _lastText(List<GTerm> msgs) {
    if (msgs.isEmpty) return '';
    final m = msgs.last;
    final s = m is GStruct ? m : GStruct('in', [m]);
    return s.args.isNotEmpty
        ? formatTerm(s.args[0]).replaceAll('_', ' ')
        : '';
  }

  // === Groups panel — the social network's groups ===========================

  Widget _groupsPanel(Panel p) {
    final v = p.groups!;
    final pending = _pendingByItem(p);
    final groups = _r.store.threads[v.threadKey] ?? const {};
    final rows = <String>{...groups.keys, ...pending.keys};
    if (rows.isEmpty) return _empty('No groups yet');
    return ListView(
      padding: const EdgeInsets.symmetric(vertical: 2),
      children: [
        for (final key in rows)
          _groupTile(v, key, pending[key], groups[key]),
      ],
    );
  }

  Widget _groupTile(
      GroupChatView v, String key, InboxCard? card, List<GTerm>? msgs) {
    final alerting = card != null;
    return ListTile(
      leading: CircleAvatar(
        backgroundColor: _accent.shade100,
        child: const Icon(Icons.groups, color: Colors.black87),
      ),
      title: Text(_groupName(key),
          style: const TextStyle(fontWeight: FontWeight.w600)),
      subtitle: alerting
          ? Text(renderTemplate(card.desc.title, card.fields),
              maxLines: 1,
              overflow: TextOverflow.ellipsis,
              style: const TextStyle(color: _accent, fontWeight: FontWeight.w600))
          : ((msgs == null || msgs.isEmpty)
              ? const Text('No messages yet',
                  style: TextStyle(color: Colors.grey))
              : Text(_lastGroupText(msgs),
                  maxLines: 1, overflow: TextOverflow.ellipsis)),
      trailing: alerting
          ? _rowAlert()
          : const Icon(Icons.chevron_right, color: Colors.grey),
      onTap: alerting
          ? () => _openCard(context, card)
          : () => setState(() => _openItem = key),
    );
  }

  Widget _groupConversation(GroupChatView v, String key) {
    final gid = tryParseTerm(key) ?? GAtom(key);
    final msgs = _r.store.threads[v.threadKey]?[key] ?? const [];
    return Scaffold(
      backgroundColor: const Color(0xFFF3EFEA),
      appBar: AppBar(
        backgroundColor: _accent,
        foregroundColor: Colors.white,
        elevation: 0,
        leading: BackButton(onPressed: () => setState(() => _openItem = null)),
        titleSpacing: 0,
        title: Text(_groupName(key),
            style: const TextStyle(fontWeight: FontWeight.bold, fontSize: 16)),
        actions: [
          for (final a in v.actions)
            TextButton(
              onPressed: () =>
                  _composeCommand(context, a, prefill: {v.groupField: gid}),
              child: Text(a.label,
                  style: const TextStyle(color: Colors.white)),
            ),
        ],
      ),
      body: Column(
        children: [
          Expanded(
            child: ListView(
              padding: const EdgeInsets.all(10),
              children: msgs.map(_groupBubble).toList(),
            ),
          ),
          _groupInputBar(v, gid),
        ],
      ),
    );
  }

  Widget _groupBubble(GTerm m) {
    // Stored as grp(author, text); render the person's own posts on the right,
    // others' on the left labeled with the author.
    final s =
        (m is GStruct && m.functor == 'grp' && m.args.length == 2) ? m : null;
    final author = s != null ? formatTerm(s.args[0]) : '';
    final text = s != null
        ? formatTerm(s.args[1]).replaceAll('_', ' ')
        : formatTerm(m).replaceAll('_', ' ');
    final outgoing = author == widget.agentId.toLowerCase();
    return Align(
      alignment: outgoing ? Alignment.centerRight : Alignment.centerLeft,
      child: Container(
        margin: const EdgeInsets.symmetric(vertical: 3),
        padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 8),
        constraints: const BoxConstraints(maxWidth: 250),
        decoration: BoxDecoration(
          color: outgoing ? const Color(0xFFEAF3DE) : Colors.white,
          borderRadius: BorderRadius.circular(10),
        ),
        child: Column(
          crossAxisAlignment:
              outgoing ? CrossAxisAlignment.end : CrossAxisAlignment.start,
          mainAxisSize: MainAxisSize.min,
          children: [
            if (!outgoing)
              Text(_cap(author),
                  style: const TextStyle(
                      fontSize: 12,
                      fontWeight: FontWeight.bold,
                      color: _accent)),
            Text(text, style: const TextStyle(fontSize: 14)),
          ],
        ),
      ),
    );
  }

  Widget _groupInputBar(GroupChatView v, GTerm groupId) {
    void send() {
      final t = _msg.text.trim();
      if (t.isEmpty) return;
      _msg.clear();
      _r.sendGroup(v, groupId, t);
    }

    return Container(
      padding: const EdgeInsets.fromLTRB(8, 6, 8, 8),
      color: Colors.white,
      child: Row(
        children: [
          const Icon(Icons.add, color: Colors.grey),
          const SizedBox(width: 6),
          Expanded(
            child: TextField(
              controller: _msg,
              onSubmitted: (_) => send(),
              decoration: InputDecoration(
                hintText: 'Message the group',
                isDense: true,
                contentPadding:
                    const EdgeInsets.symmetric(horizontal: 14, vertical: 10),
                border: OutlineInputBorder(
                    borderRadius: BorderRadius.circular(20),
                    borderSide: BorderSide(color: Colors.grey.shade300)),
              ),
            ),
          ),
          IconButton(
              onPressed: send, icon: const Icon(Icons.send, color: _accent)),
        ],
      ),
    );
  }

  /// A group's display name: `group_id(creator, name)` → the name; else the key.
  String _groupName(String key) {
    final t = tryParseTerm(key);
    if (t is GStruct && t.functor == 'group_id' && t.args.length == 2) {
      return _cap(formatTerm(t.args[1]));
    }
    return key;
  }

  String _lastGroupText(List<GTerm> msgs) {
    final m = msgs.last;
    if (m is GStruct && m.functor == 'grp' && m.args.length == 2) {
      final who = _cap(formatTerm(m.args[0]));
      final text = formatTerm(m.args[1]).replaceAll('_', ' ');
      return '$who: $text';
    }
    return formatTerm(m).replaceAll('_', ' ');
  }

  // === Compose forms — the "+" (Request-shaped clauses) =====================

  void _composeSheet(BuildContext context, List<CommandDesc> commands) {
    if (commands.length == 1) {
      _composeCommand(context, commands.first);
      return;
    }
    showModalBottomSheet<void>(
      context: context,
      builder: (ctx) => SafeArea(
        child: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            for (final c in commands)
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

  Future<void> _composeCommand(BuildContext context, CommandDesc cmd,
      {Map<String, GTerm> prefill = const {}}) async {
    // Fields fixed by context (e.g. the friend whose wallet is open) are not
    // shown; the rest are entered.
    final shown = cmd.args.where((f) => !prefill.containsKey(f.name)).toList();
    final controllers = {
      for (final f in shown) f.name: TextEditingController(),
    };
    final ok = await showDialog<bool>(
      context: context,
      builder: (ctx) => AlertDialog(
        title: Text(cmd.label),
        content: Column(
          mainAxisSize: MainAxisSize.min,
          children: shown
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

    final values = <String, GTerm>{...prefill};
    for (final f in shown) {
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
        return GAtom(chatAtom(raw));
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

  String _cap(String s) => s.isEmpty ? s : s[0].toUpperCase() + s.substring(1);

  String _initial(String s) => s.isEmpty ? '?' : s[0].toUpperCase();
}
