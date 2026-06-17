/// Generic two-surface agent UI (paper §7.4): Inbox + Outbox, with declared
/// activity state (e.g. the Friends list) rendered between them.
///
/// Renders strictly from a [UiRuntime] and its [Manifest]; no app-specific
/// constructor name appears here.
library;

import 'package:flutter/material.dart';

import 'manifest.dart';
import 'runtime.dart';
import 'term.dart';

class AgentSurface extends StatelessWidget {
  final String agentId;
  final UiRuntime runtime;
  const AgentSurface({super.key, required this.agentId, required this.runtime});

  Manifest get _m => runtime.manifest;

  @override
  Widget build(BuildContext context) {
    return Column(
      crossAxisAlignment: CrossAxisAlignment.stretch,
      children: [
        _header(),
        Expanded(
          child: ListView(
            padding: const EdgeInsets.all(8),
            children: [
              ..._m.state.map(_stateView),
              _sectionLabel('Inbox'),
              if (runtime.inbox.isEmpty) _empty('No requests')
              else ...runtime.inbox.map((c) => _card(context, c)),
              if (runtime.store.feed.isNotEmpty) ...[
                _sectionLabel('Activity'),
                ...runtime.store.feed.reversed.take(8).map(_feedLine),
              ],
            ],
          ),
        ),
        _outbox(context),
      ],
    );
  }

  Widget _header() => Container(
        padding: const EdgeInsets.symmetric(horizontal: 8, vertical: 6),
        color: Colors.orange,
        child: Text(
          agentId,
          style: const TextStyle(
              color: Colors.white, fontWeight: FontWeight.bold, fontSize: 14),
        ),
      );

  Widget _sectionLabel(String text) => Padding(
        padding: const EdgeInsets.only(top: 12, bottom: 4),
        child: Text(text.toUpperCase(),
            style: TextStyle(
                fontSize: 11,
                fontWeight: FontWeight.bold,
                color: Colors.grey.shade600,
                letterSpacing: 0.5)),
      );

  Widget _empty(String text) => Padding(
        padding: const EdgeInsets.symmetric(vertical: 4),
        child: Text(text,
            style: TextStyle(
                fontSize: 12,
                fontStyle: FontStyle.italic,
                color: Colors.grey.shade500)),
      );

  Widget _feedLine(String line) => Padding(
        padding: const EdgeInsets.symmetric(vertical: 2),
        child: Text(line, style: const TextStyle(fontSize: 12)),
      );

  Widget _stateView(StateView v) {
    switch (v.kind) {
      case StateKind.list:
        final items = runtime.store.lists[v.key] ?? const [];
        return Column(
          crossAxisAlignment: CrossAxisAlignment.stretch,
          children: [
            _sectionLabel(v.label),
            if (items.isEmpty)
              _empty('None')
            else
              Wrap(
                spacing: 6,
                runSpacing: 6,
                children: items
                    .map((t) => Chip(
                          label: Text(formatTerm(t)),
                          backgroundColor: Colors.orange.shade50,
                          visualDensity: VisualDensity.compact,
                        ))
                    .toList(),
              ),
          ],
        );
      case StateKind.value:
        final v0 = runtime.store.values[v.key];
        return Column(
          crossAxisAlignment: CrossAxisAlignment.stretch,
          children: [
            _sectionLabel(v.label),
            _feedLine(v0 == null ? '—' : formatTerm(v0)),
          ],
        );
      case StateKind.thread:
        final t = runtime.store.threads[v.key] ?? const {};
        return Column(
          crossAxisAlignment: CrossAxisAlignment.stretch,
          children: [
            _sectionLabel(v.label),
            if (t.isEmpty)
              _empty('None')
            else
              ...t.entries.map((e) => _feedLine(
                  '${e.key}: ${e.value.map(formatTerm).join(", ")}')),
          ],
        );
    }
  }

  Widget _card(BuildContext context, InboxCard card) {
    return Card(
      margin: const EdgeInsets.symmetric(vertical: 4),
      child: Padding(
        padding: const EdgeInsets.all(10),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text(renderTemplate(card.desc.title, card.fields),
                style: const TextStyle(fontWeight: FontWeight.bold, fontSize: 14)),
            if (card.desc.subtitle != null)
              Padding(
                padding: const EdgeInsets.only(top: 2),
                child: Text(renderTemplate(card.desc.subtitle!, card.fields),
                    style: const TextStyle(fontSize: 12)),
              ),
            const SizedBox(height: 8),
            Wrap(
              spacing: 8,
              children: card.desc.answers
                  .map((a) => ElevatedButton(
                        onPressed: () => _answer(context, card, a),
                        child: Text(a.label),
                      ))
                  .toList(),
            ),
          ],
        ),
      ),
    );
  }

  void _answer(BuildContext context, InboxCard card, AnswerDesc answer) {
    if (answer.needsPicker) {
      // GSG v1 has no picker answers; reserved for child-safe.
      return;
    }
    runtime.answerCard(card, answer);
  }

  Widget _outbox(BuildContext context) {
    return Container(
      padding: const EdgeInsets.all(8),
      color: Colors.orange.shade50,
      child: Wrap(
        spacing: 8,
        runSpacing: 8,
        children: _m.commands
            .map((c) => ElevatedButton.icon(
                  icon: const Icon(Icons.add, size: 16),
                  label: Text(c.label),
                  onPressed: () => _composeCommand(context, c),
                ))
            .toList(),
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
          TextButton(onPressed: () => Navigator.pop(ctx, false), child: const Text('Cancel')),
          ElevatedButton(onPressed: () => Navigator.pop(ctx, true), child: const Text('Send')),
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
    runtime.submitCommand(cmd, values);
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
}
