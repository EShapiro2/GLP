/// A walk through contracts' apps drawn from interface descriptions, every agent GLP's: for each app the list
/// names, three persons, Alice, Bob and Carol, a conversation between each two, each running the contract's
/// mini-app; at each step one of them takes an act her described screens offer, filling what is left, and
/// everyone asked says yes.  At each step it compares what the screens offer with what the platform's request
/// widget offers (each way of each act the person's state satisfies), and it records each request refused,
/// each goal that fails and each act the screens offer that the platform does not.
///
/// The list, `DV_LIST`, one app a line: its directory under `programs/` and its description's compiled form
/// under `programs/`.  `DV_STEPS` steps (30), seed `DV_SEED` (1); the report, a JSON line per app, to
/// `DV_OUT`.  Nothing runs where `DV_LIST` is not set.
library;

import 'dart:convert';
import 'dart:io';
import 'dart:math';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart' show runtimeTermOf, screenTermOf;
import 'package:glp_multiagent/ui_runtime/bridge.dart';
import 'package:glp_multiagent/ui_runtime/described.dart';
import 'package:glp_multiagent/ui_runtime/manifest.dart';
import 'package:glp_multiagent/ui_runtime/named_widgets.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/external_io.dart' show InputInjector;
import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell;
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:glp_runtime/vglp/canonical.dart' show DeclaredAct;

import 'programs_dir.dart';

class _P {
  final String name;
  final InputInjector grants;
  final HeapCell draws;
  final List<GTerm> unsent = [];
  final Set<int> answered = {};
  int fed = 0;
  late final UiRuntime ui = UiRuntime(
      manifest: const Manifest(title: 'app', panels: [Panel(id: 'app', name: 'app', constructs: true)], activity: []),
      onSend: unsent.add);
  _P(this.name, this.grants, this.draws);
  SocialState? get state => SocialState.read(ui.bridge.construct(0)?.view).$2;
  OpenConstruct? get request {
    for (final c in ui.bridge.constructs) {
      if (namedWidget(c.widget)?.$1 == socialMenuName) return c;
    }
    return null;
  }
}

Future<Map<String, Object?>> _walk(String programs, String dir, String uiPath, int steps, Random rnd) async {
  final report = <String, Object?>{'app': dir, 'ui': uiPath};
  final problems = <String>[];
  report['problems'] = problems;
  final trace = <String>[];
  final ui = UiDescription.parse(File('$programs/$uiPath').readAsStringSync());
  final engine = GlpEngine(rootSelfGlpPath: '$programs/self.glp');
  if (!engine.loadProgram('$programs/$dir')) {
    problems.add('load: programs/$dir does not load');
    return report;
  }
  final names = const ['alice', 'bob', 'carol', 'dave', 'erin'].take(int.parse(Platform.environment['DV_PERSONS'] ?? '3')).toList();
  String conv(String p, String q) => 'conversation($q, ch(C_${q}_$p?, C_${p}_$q))';
  final goals = [
    for (var i = 0; i < names.length; i++)
      'miniapp(${names[i]}, ch(G$i?, D$i), ch([${[for (final q in names) if (q != names[i]) conv(names[i], q)].join(', ')}], U$i))'
  ];
  final play = engine.postGoal(goals.join(', '), inputs: [for (var i = 0; i < names.length; i++) 'G$i']);
  final ps = [for (var i = 0; i < names.length; i++) _P(names[i], play.inputs['G$i']!, play.variables['D$i']!)];

  var debugNow = false;
  bool drain() {
    final r = play.scheduler.drainToQuiescence(maxCycles: 20000000, debug: debugNow);
    if (r.status == ExecutionStatus.failed) {
      problems.add('a goal failed');
      return false;
    }
    if (r.status == ExecutionStatus.capped) {
      problems.add('the run did not quiesce');
      return false;
    }
    return true;
  }

  rt.Term deep(rt.Term t) {
    final d = engine.runtime.heap.dereference(t);
    return d is rt.StructTerm ? rt.StructTerm(d.functor, [for (final a in d.args) deep(a)]) : d;
  }

  void feed() {
    for (final p in ps) {
      var cell = engine.runtime.heap.dereference(rt.VarRef(p.draws));
      var i = 0;
      while (cell is rt.StructTerm && cell.functor == '.' && cell.args.length == 2) {
        if (i++ >= p.fed) {
          p.ui.handle(screenTermOf(deep(cell.args[0])));
          p.fed++;
        }
        cell = engine.runtime.heap.dereference(cell.args[1]);
      }
    }
  }

  /// Grants delivered and every question answered yes, until nothing moves.
  bool settle() {
    for (var round = 0; round < 200; round++) {
      var moved = false;
      for (final p in ps) {
        while (p.unsent.isNotEmpty) {
          moved = true;
          p.grants.inject(runtimeTermOf(p.unsent.removeAt(0))).forEach(engine.runtime.gq.enqueue);
          if (!drain()) return false;
        }
      }
      feed();
      for (final p in ps) {
        for (final c in p.ui.bridge.constructs) {
          if (c.id == 0 || ctorArgs(c.view).$1 != 'approval' || p.answered.contains(c.id)) continue;
          final at = questionAt(ctorArgs(c.view).$2.last);
          if (at == null) continue;
          p.answered.add(c.id);
          p.ui.bridge.grant(c.id, at, const GAtom('yes'));
          moved = true;
        }
      }
      if (!moved) return true;
    }
    problems.add('settle: still moving after 200 rounds');
    return false;
  }

  if (!drain()) return report;
  feed();
  final taken = <String, int>{}, refused = <String, int>{}, skipped = <String, int>{};
  void skip(String why) => skipped[why] = (skipped[why] ?? 0) + 1;
  final missing = <String>{}, extra = <String>{};
  final why = <String>[];
  final selfRefused = <String, int>{};
  var acts = 0;

  String wayKey(DeclaredAct a, Map<int, GTerm> w) =>
      '${a.schema}(${[for (final p in a.binds) w[p] == null ? '_' : formatTerm(w[p]!)].join(',')})';

  for (var step = 0; step < steps; step++) {
    final p = ps[rnd.nextInt(ps.length)];
    final st = p.state, rq = p.request;
    if (st == null || rq == null) { skip('no view or no request question'); continue; }
    final ask = SocialAsk(st.me, questionAt(rq.view), RequestOffers.of(rq.widget), (path, raw) => p.ui.bridge.grant(rq.id, path, raw));
    if (ask.at == null) { skip('request question not open'); continue; }
    final core = DescribedCore(ui, st, ask);

    // What the platform offers: every way of every act the person proposes that her state satisfies.
    final platform = <String>{};
    for (final b in ask.offers.buttons) {
      for (final a in b.acts) {
        // A place of the request that is neither the proposer's, nor bound by her atoms, nor a field: no
        // request can be formed.
        final key = ask.offers.keyOf(a.schema);
        final n = key == null ? 0 : int.parse(key.substring(key.lastIndexOf('/') + 1));
        for (var k = 1; k <= n; k++) {
          if (k != a.proposer && !a.binds.contains(k) && !a.fields.any((f) => f.place == k)) {
            problems.add('${a.schema}: place $k of its request is neither the proposer\'s, bound, nor a field');
          }
        }
        final info = ActInfo(a);
        final ws = info.ways(st.atoms, st.me);
        for (final w in ws) {
          if (info.lacks(st.atoms, {...w, a.proposer: st.me})) platform.add(wayKey(a, w));
        }
      }
    }
    // What the screens offer, over every screen she can reach.
    final offers = <(UiComponent, DeclaredAct, DescribedWay, GTerm?)>[];
    final described = <String>{};
    for (final (name, subject) in core.screens()) {
      for (final c in ui.screens[name]!.components) {
        if (c.kind != 'action') continue;
        final a = core.act(c);
        if (a == null) {
          problems.add('${c.schema}: the platform declares no act at place ${c.me}');
          continue;
        }
        final key = ask.offers.keyOf(a.schema);
        final n = key == null ? -1 : int.parse(key.substring(key.lastIndexOf('/') + 1));
        if (n < c.arity) problems.add('${c.schema}: the description has ${c.arity} places, the request $n');
        for (final w in core.ways(c, a, subject)) {
          described.add(wayKey(a, w));
          offers.add((c, a, w, subject));
        }
      }
    }
    for (final k in platform.difference(described)) {
      missing.add(k.replaceAll(RegExp(r'\(.*'), ''));
    }
    for (final k in described.difference(platform)) {
      extra.add(k.replaceAll(RegExp(r'\(.*'), ''));
    }
    if (offers.isEmpty) { skip('nothing offered'); continue; }

    // One of them, its fields filled.
    final (c, a, w, subject) = offers[rnd.nextInt(offers.length)];
    final given = <int, GTerm>{};
    var ok = true;
    for (final f in core.fieldsOf(a, w)) {
      final src = c.places[f.place];
      if (src?.from != null) {
        final xs = src!.from == 'everyone' ? core.everyone() : core.valuesOf(src.from!);
        if (xs.isEmpty) {
          ok = false;
          break;
        }
        given[f.place] = xs[rnd.nextInt(xs.length)];
        continue;
      }
      final strangers = [for (final q in names) if (q != p.name && !st.atoms.any((x) => formatTerm(x) == 'knows($q)')) q];
      final others = [for (final q in names) if (q != p.name) q];
      final text = switch (f.type) {
        'key' => strangers.isEmpty ? '' : strangers[rnd.nextInt(strangers.length)],
        'person' || 'agent' => others[rnd.nextInt(others.length)],
        'number' => '${1 + rnd.nextInt(3)}',
        'set' => 'x${rnd.nextInt(3)}',
        _ => 'w${rnd.nextInt(4)}',
      };
      final v = core.textValue(f, text, schema: a.schema);
      if (v == null) {
        ok = false;
        break;
      }
      given[f.place] = v;
    }
    if (!ok) { skip('a field not filled: ${a.schema}'); continue; }
    final r = core.request(a, {...w, ...given});
    if (r == null) { skip('no request formed: ${a.schema}'); continue; }
    final before = st.atoms.map(formatTerm).toSet();
    final noticesBefore = st.notices.length;
    ask.propose(r);
    acts++;
    debugNow = '$acts' == Platform.environment['DV_DEBUG_ACT'];
    trace.add('${p.name}: ${formatTerm(r)}');
    if (!settle()) {
      // The acts that led to it, and each person's facts, for the report.
      report['trace'] = trace;
      report['facts'] = {for (final q in ps) q.name: [for (final a in q.state?.atoms ?? const <GTerm>[]) formatTerm(a)]};
      break;
    }
    final after = p.state;
    final changed = after != null && after.atoms.map(formatTerm).toSet().difference(before).isNotEmpty ||
        after != null && before.difference(after.atoms.map(formatTerm).toSet()).isNotEmpty;
    final refusedNow = after != null && after.notices.length > noticesBefore;
    if (refusedNow) {
      refused[a.schema] = (refused[a.schema] ?? 0) + 1;
      final n = after.notices.last;
      // Refused by the proposer's own machine: the screens offered what her own machine does not allow.
      if (formatTerm(n.by) == p.name) selfRefused['${a.schema}${n.declined ? '' : n.lacks.isEmpty && n.has.isEmpty ? ' (no reason)' : ''}'] = (selfRefused['${a.schema}${n.declined ? '' : n.lacks.isEmpty && n.has.isEmpty ? ' (no reason)' : ''}'] ?? 0) + 1;
      if (why.length < 12) {
        why.add('${p.name}: ${formatTerm(r)} refused by ${formatTerm(n.by)}${n.declined ? ' (declined)' : ', lacking ${n.lacks.map(formatTerm).join(' ')} having ${n.has.map(formatTerm).join(' ')}'}');
      }
    } else if (changed) {
      taken[a.schema] = (taken[a.schema] ?? 0) + 1;
    }
  }
  report['problems'] = problems.toSet().toList();
  report['acts'] = acts;
  report['taken'] = taken;
  report['refused'] = refused;
  report['skipped'] = skipped;
  report['why'] = why;
  report['selfRefused'] = selfRefused;
  report['persons'] = names.length;
  report['missing'] = missing.toList()..sort();
  report['extra'] = extra.toList()..sort();
  return report;
}

void main() {
  final list = Platform.environment['DV_LIST'];
  test('described screens walked over contracts\' apps', () async {
    if (list == null) return;
    final programs = programsDir();
    final steps = int.parse(Platform.environment['DV_STEPS'] ?? '30');
    final out = File(Platform.environment['DV_OUT'] ?? '/tmp/dv-report.jsonl');
    out.writeAsStringSync('');
    for (final line in File(list).readAsLinesSync()) {
      final xs = line.trim().split(RegExp(r'\s+'));
      if (xs.length < 2) continue;
      Map<String, Object?> r;
      try {
        r = await _walk(programs, xs[0], xs[1], steps, Random(int.parse(Platform.environment['DV_SEED'] ?? '1')));
      } catch (e, st) {
        r = {'app': xs[0], 'problems': ['exception: $e', '$st'.split('\n').take(4).join(' | ')]};
      }
      out.writeAsStringSync('${jsonEncode(r)}\n', mode: FileMode.append);
    }
  }, timeout: Timeout.none);
}
