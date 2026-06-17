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
  /// 0 = the state/outbox surface (Friends or Chats), 1 = the inbox (Requests).
  int _tab = 0;

  /// When on a chat app, the open conversation peer (null = the chat list).
  String? _openPeer;

  final TextEditingController _msg = TextEditingController();

  Manifest get _m => widget.runtime.manifest;
  UiRuntime get _r => widget.runtime;

  static const MaterialColor _accent = Colors.orange;

  bool get _chats => _m.chat != null;
  bool get _wallet => _m.wallet != null;

  @override
  Widget build(BuildContext context) {
    // Drill-down (chats view: conversation; wallet view: a person's holdings).
    if (_tab == 0 && _openPeer != null) {
      if (_chats) return _conversation(_m.chat!, _openPeer!);
      if (_wallet) return _walletDetail(_m.wallet!, _openPeer!);
    }

    final requests = _r.inbox.length;
    final onState = _tab == 0;
    final stateLabel = _chats ? 'Chats' : (_wallet ? _m.wallet!.label : 'Friends');
    return Scaffold(
      backgroundColor: Colors.white,
      appBar: AppBar(
        backgroundColor: _accent,
        foregroundColor: Colors.white,
        elevation: 0,
        title: Text(onState ? _m.title : 'Requests',
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
      body: onState ? _stateScreen() : _requestsScreen(),
      floatingActionButton: onState && !_wallet
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
          NavigationDestination(
              icon: Icon(_chats
                  ? Icons.chat_bubble_outline
                  : (_wallet
                      ? Icons.account_balance_wallet_outlined
                      : Icons.people_outline)),
              selectedIcon: Icon(_chats
                  ? Icons.chat_bubble
                  : (_wallet
                      ? Icons.account_balance_wallet
                      : Icons.people)),
              label: stateLabel),
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

  // === State surface — the state the outbox leaves ==========================

  Widget _stateScreen() => _chats
      ? _chatList(_m.chat!)
      : (_wallet ? _walletList(_m.wallet!) : _friendsList());

  // --- Wallet (coins): person + friends, each drilling into their holdings ---
  Widget _walletList(WalletView w) {
    final people = <String>[w.selfKey, ...(_r.store.lists[w.friendsList] ?? const [])
        .map(formatTerm)];
    return ListView(
      padding: const EdgeInsets.symmetric(vertical: 2),
      children: [
        for (final p in people)
          ListTile(
            leading: CircleAvatar(
              backgroundColor: _accent.shade100,
              child: Text(_initial(p),
                  style: const TextStyle(
                      color: Colors.black87, fontWeight: FontWeight.bold)),
            ),
            title: Text(p == w.selfKey ? 'You' : _cap(p),
                style: const TextStyle(fontWeight: FontWeight.w600)),
            subtitle: Text(_holdingsSummary(w, p),
                maxLines: 1, overflow: TextOverflow.ellipsis),
            trailing: const Icon(Icons.chevron_right, color: Colors.grey),
            onTap: () => setState(() => _openPeer = p),
          ),
      ],
    );
  }

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
        leading: BackButton(onPressed: () => setState(() => _openPeer = null)),
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
                              style: const TextStyle(fontWeight: FontWeight.w600)),
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

  Widget _friendsList() {
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
          break; // chat list handled by _chatList when chat is declared
      }
    }
    if (children.isEmpty) children.add(_empty('Nothing yet'));
    return ListView(
        padding: const EdgeInsets.symmetric(vertical: 4), children: children);
  }

  // --- Chat list (conversations) ---
  Widget _chatList(ChatView chat) {
    final convs = _r.store.threads[chat.threadKey] ?? const {};
    if (convs.isEmpty) return _empty('No chats yet');
    final peers = convs.keys.toList();
    return ListView(
      padding: const EdgeInsets.symmetric(vertical: 2),
      children: [
        for (final p in peers)
          ListTile(
            leading: CircleAvatar(
              backgroundColor: _accent.shade100,
              child: Text(_initial(p),
                  style: const TextStyle(
                      color: Colors.black87, fontWeight: FontWeight.bold)),
            ),
            title: Text(_cap(p),
                style: const TextStyle(fontWeight: FontWeight.w600)),
            subtitle: Text(_lastText(convs[p]!),
                maxLines: 1, overflow: TextOverflow.ellipsis),
            onTap: () => setState(() => _openPeer = p),
          ),
      ],
    );
  }

  // --- Open conversation: bubbles + input ---
  Widget _conversation(ChatView chat, String peer) {
    final msgs = _r.store.threads[chat.threadKey]?[peer] ?? const [];
    return Scaffold(
      backgroundColor: const Color(0xFFF3EFEA),
      appBar: AppBar(
        backgroundColor: _accent,
        foregroundColor: Colors.white,
        elevation: 0,
        leading: BackButton(onPressed: () => setState(() => _openPeer = null)),
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
    // Stored as out(text[,tick]) / in(text).
    final s = m is GStruct ? m : GStruct('in', [m]);
    final outgoing = s.functor == 'out';
    final text = s.args.isNotEmpty ? formatTerm(s.args[0]) : '';
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
    return s.args.isNotEmpty ? formatTerm(s.args[0]) : '';
  }

  String _cap(String s) => s.isEmpty ? s : s[0].toUpperCase() + s.substring(1);

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
