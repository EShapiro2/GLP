// glp_runtime/lib/sglp/log.dart
//
// The log of an sGLP run (svGLP, sections/sglp.tex, Definition "Interface
// Variable, Log"):
//
//   In a run of the simulation program of (M, P, pi), the interface variables
//   of an agent a are its interactive variables and the variables of every
//   term assigned to an interface variable of a.  The log of the run is the
//   sequence, in the order of the run, of the triples (t, a, X := s), one for
//   each assignment X := s that a Reduce at a makes to an interface variable X
//   of a, t the simulated time of that Reduce.
//
// An interactive variable of a is registered by person/2 at the Ask
// (lib/sglp/person.dart).  The heap reports every assignment
// (HeapFCP.onAssign): X := s binds X's writer to s, or X := Y? binds it to a
// reader.  At each, if X is an interface variable of some agents, the
// unbound variables of s become interface variables of each of them --- by
// the Definition, whoever made the assignment --- and if the Reduce making it
// is at one of them, the entry is written.
//
// The assignments of one Reduce --- its writer substitution, which the
// machine applies one binding at a time (runtime/commit.dart), and those of
// the kernels its body runs --- are taken together, as the Reduce's
// substitution is one: at the end of the Reduce, each in the order it was
// made, with s as the heap holds it then.  A variable the Reduce itself
// assigned is thus printed as its value, and the variables of s are those
// still unbound when the Reduce is done; the intermediate variables by which
// the machine builds a clause's terms never reach the log.
//
// FORMAT, one entry per line, three fields separated by tabs:
//
//   t <TAB> a <TAB> X := s
//
//   t   the simulated time, in seconds, as Dart prints a double (the shortest
//       text that reads back to the same double)
//   a   the agent, 1..N
//   X   the assigned variable, V<k>
//   s   the term, in GLP syntax: a variable V<k>, followed by ? where s holds
//       its reader; a constant that is not a plain atom quoted
//
// V<k> names the k-th variable of the run to become an interface variable of
// some agent, so the names are the run's and not the heap's: two runs with
// one seed write the same bytes.

import '../runtime/heap_fcp.dart';
import '../runtime/terms.dart';

class SimLog {
  final HeapFCP heap;

  /// Where each entry goes, without its line end.
  final void Function(String line) emit;

  /// The simulated time.
  final double Function() clock;

  /// Each interface variable, by its writer's address, with the agents of
  /// which it is one.
  final Map<int, List<int>> _agents = {};

  /// The name of each interface variable: k of `V<k>`.
  final Map<int, int> _names = {};
  int _nextName = 1;

  /// Entries written.
  int entries = 0;

  /// The assignments to interface variables the current Reduce has made, in
  /// order; taken at its end ([endReduce]).
  final List<(int, Term)> _pending = [];
  bool _inReduce = false;

  SimLog({
    required this.heap,
    required this.emit,
    required this.clock,
  });

  /// The number of variables that are interface variables of some agent now.
  int get interfaceVariables => _agents.length;

  /// Register the variable [x] as an interactive variable of [agent].  [x] is
  /// a variable's writer or reader; false where it is not an unbound variable.
  bool register(int agent, Term x) {
    if (x is! VarRef) return false;
    final end = heap.derefAddr(x.addr);
    if (end is! VarRef) return false;
    _add(end.addr, agent);
    return true;
  }

  /// True if the variable [x] is an interface variable of [agent].
  bool isInterfaceOf(int agent, Term x) {
    if (x is! VarRef) return false;
    final end = heap.derefAddr(x.addr);
    if (end is! VarRef) return false;
    return _agents[end.addr]?.contains(agent) ?? false;
  }

  void _add(int writer, int agent) {
    final list = _agents.putIfAbsent(writer, () => <int>[]);
    if (!list.contains(agent)) list.add(agent);
    _names.putIfAbsent(writer, () => _nextName++);
  }

  /// A Reduce begins: its assignments are held until [endReduce].
  void beginReduce() {
    _inReduce = true;
  }

  /// The Reduce ends: its assignments to interface variables are taken, in
  /// the order it made them, and those it made at an agent of the variable
  /// are written.  [agent] is the Reduce's agent, or null for a goal at none.
  void endReduce(int? agent) {
    _inReduce = false;
    if (_pending.isEmpty) return;
    for (final (w, value) in _pending) {
      _take(w, value, agent);
    }
    _pending.clear();
  }

  /// The heap's report of the assignment of the writer at [writerAddr] with
  /// [value]: a term, or a VarRef to a reader.
  void onAssign(int writerAddr, Term value) {
    if (!_agents.containsKey(writerAddr)) return;
    if (_inReduce) {
      _pending.add((writerAddr, value));
    } else {
      // An assignment outside any Reduce is no agent's: it widens the
      // interface, and no entry is written.
      _take(writerAddr, value, null);
    }
  }

  void _take(int writerAddr, Term value, int? agent) {
    final agents = _agents.remove(writerAddr);
    if (agents == null) return;
    final name = _names.remove(writerAddr);

    // The variables of s become interface variables of each agent of X.
    final vars = <int>{};
    _variablesOf(value, vars, 0);
    for (final a in agents) {
      for (final v in vars) {
        _add(v, a);
      }
    }

    if (agent == null || !agents.contains(agent)) return;
    entries++;
    emit('${clock()}\t$agent\tV$name := ${_print(value, 0)}');
  }

  /// Guard against a term too deep to be one: GLP terms are finite, and a
  /// cycle is an SRSW violation the heap reports elsewhere.
  static const int _maxDepth = 100000;

  /// The unbound variables of [t], by writer address, in order of occurrence.
  void _variablesOf(Term t, Set<int> out, int depth) {
    var cur = t;
    var d = depth;
    // Iterate down the last argument (a list's tail) to keep long lists flat.
    while (true) {
      if (d > _maxDepth) return;
      if (cur is VarRef) {
        final end = heap.derefAddr(cur.addr);
        if (end is VarRef) {
          out.add(end.addr);
          return;
        }
        if (end is! Term) return; // an imported variable: not of this machine
        cur = end;
        continue;
      }
      if (cur is StructTerm) {
        if (cur.args.isEmpty) return;
        for (var i = 0; i < cur.args.length - 1; i++) {
          _variablesOf(cur.args[i], out, d + 1);
        }
        cur = cur.args.last;
        d++;
        continue;
      }
      return;
    }
  }

  /// [t] in GLP syntax, variables named.
  String _print(Term t, int depth) {
    if (depth > _maxDepth) return '...';
    if (t is VarRef) {
      final end = heap.derefAddr(t.addr);
      if (end is VarRef) {
        final k = _names[end.addr];
        final n = k == null ? '_' : 'V$k';
        // The term holds the reader where the occurrence is not the unbound
        // writer itself: a reader, or a writer bound to a reader.
        return end.addr == t.addr ? n : '$n?';
      }
      if (end is! Term) return '_';
      return _print(end, depth + 1);
    }
    if (t is ConstTerm) return _constant(t.value);
    if (t is StructTerm) {
      if (t.functor == '.' && t.args.length == 2) return _list(t, depth);
      if (t.args.isEmpty) return _atom(t.functor);
      return '${_atom(t.functor)}('
          '${t.args.map((a) => _print(a, depth + 1)).join(', ')})';
    }
    return t.toString();
  }

  String _list(StructTerm t, int depth) {
    final items = <String>[];
    Term cur = t;
    while (true) {
      if (cur is VarRef) {
        final end = heap.derefAddr(cur.addr);
        if (end is Term && end is! VarRef) {
          cur = end;
          continue;
        }
        return '[${items.join(', ')} | ${_print(cur, depth + 1)}]';
      }
      if (cur is StructTerm && cur.functor == '.' && cur.args.length == 2) {
        items.add(_print(cur.args[0], depth + 1));
        cur = cur.args[1];
        continue;
      }
      if (cur is ConstTerm && (cur.value == 'nil' || cur.value == null)) {
        return '[${items.join(', ')}]';
      }
      return '[${items.join(', ')} | ${_print(cur, depth + 1)}]';
    }
  }

  static final RegExp _plainAtom = RegExp(r'^[a-z][A-Za-z0-9_]*$');

  static String _constant(Object? v) {
    if (v == null || v == 'nil') return '[]';
    if (v is String) return _atom(v);
    return '$v';
  }

  static String _atom(String s) {
    if (_plainAtom.hasMatch(s)) return s;
    return "'${s.replaceAll(r'\', r'\\').replaceAll("'", r"\'")}'";
  }
}
