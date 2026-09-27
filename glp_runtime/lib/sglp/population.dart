// glp_runtime/lib/sglp/population.dart
//
// The population of an sGLP run (svGLP, sections/sglp.tex, Definition "Dual,
// Person Procedure, Person Declaration, Kind, Dimension, Population"):
//
//   The dual T° of an interactive type T is T'? where T is T', and T' where T
//   is T'?.  A person procedure of T is a binary sGLP procedure declared
//   `procedure p(T°, Integer?)`, its second argument a seed, and a person
//   declaration `T =::= p` names one.  A kind is a name with an sGLP program
//   and a set of person declarations, one for each of a set of interactive
//   types, naming procedures of that program; two kinds lie in one dimension
//   if they declare the same interactive types.  A population of n agents
//   assigns each dimension a probability distribution over its kinds and has a
//   seed; each agent draws one kind per dimension, and the union of the kinds
//   it draws is its stochastic person.
//
// [checkPopulation] makes the three checks the run declaration needs --- each
// person procedure declared p(T°, Integer?), the kinds of a dimension
// declaring the same interactive types, each dimension's probabilities summing
// to one --- with the well-formedness they presuppose.  [Population] is the
// run declaration as the engine holds it: the agents 1..n, each agent's kinds
// drawn from the seed, and the seed a person goal is handed.

import '../analysis/type_checker/type_ast.dart' show TypeRef, ProcDecl;
import '../compiler/ast.dart';
import 'draws.dart' as draws;

/// Probabilities are reals as written, and 0.7 + 0.3 is not 1 in binary
/// floating point: a dimension's probabilities sum to one within this.
const double probabilityTolerance = 1e-9;

/// One kind as the engine holds it: its person procedure for each
/// interactive type it declares, and the module whose section declares it.
class Kind {
  final String name;
  final String module;
  final Map<String, String> personProcedures; // type key -> procedure
  Kind(this.name, this.module, this.personProcedures);
}

/// A dimension: its name and its distribution over kinds.
class Dimension {
  final String name;
  final List<String> kinds;
  final List<double> probabilities;
  Dimension(this.name, this.kinds, this.probabilities);
}

/// A population of [agents] agents, numbered 1..[agents].
class Population {
  final int agents;
  final int seed;
  final double untilSeconds;
  final String untilText;
  final List<Dimension> dimensions;
  final Map<String, Kind> kinds;

  /// Agent a's kind in dimension d is `_kindIndex[a - 1][d]`.
  late final List<List<int>> _kindIndex = [
    for (var a = 1; a <= agents; a++)
      [for (var d = 0; d < dimensions.length; d++) _draw(a, d)]
  ];

  Population({
    required this.agents,
    required this.seed,
    required this.untilSeconds,
    required this.untilText,
    required this.dimensions,
    required this.kinds,
  });

  int _draw(int agent, int d) {
    final dim = dimensions[d];
    final u = draws.kindDraw(seed, agent, d);
    var acc = 0.0;
    for (var i = 0; i < dim.probabilities.length; i++) {
      acc += dim.probabilities[i];
      if (u < acc) return i;
    }
    // The probabilities sum to one within the tolerance; a draw in the gap
    // their rounding leaves goes to the last kind with a positive one.
    for (var i = dim.probabilities.length - 1; i >= 0; i--) {
      if (dim.probabilities[i] > 0) return i;
    }
    return dim.probabilities.length - 1;
  }

  void _checkAgent(int agent) {
    if (agent < 1 || agent > agents) {
      throw RangeError.range(agent, 1, agents, 'agent');
    }
  }

  /// Agent [agent]'s kind in dimension [dimension] (an index into
  /// [dimensions]).
  String kindOf(int agent, int dimension) {
    _checkAgent(agent);
    return dimensions[dimension].kinds[_kindIndex[agent - 1][dimension]];
  }

  /// Agent [agent]'s kinds, one per dimension, in the order of [dimensions].
  List<String> kindsOf(int agent) =>
      [for (var d = 0; d < dimensions.length; d++) kindOf(agent, d)];

  /// The person procedure agent [agent]'s stochastic person declares for the
  /// interactive type [typeKey] (`Menu`, `Menu?`), with the kind and module
  /// declaring it; null if none of its kinds declares the type.
  ({String procedure, String kind, String module})? personProcedure(
      int agent, String typeKey) {
    for (final k in kindsOf(agent)) {
      final kind = kinds[k]!;
      final p = kind.personProcedures[typeKey];
      if (p != null) return (procedure: p, kind: k, module: kind.module);
    }
    return null;
  }

  /// The seed handed to the person goal of the goal identified by
  /// [askedGoal] at agent [agent] (Definition "Simulation Program").
  int personSeed(int agent, int askedGoal) {
    _checkAgent(agent);
    return draws.personSeed(seed, agent, askedGoal);
  }
}

/// A population declaration found wanting, with where.
class PopulationError {
  final String message;
  final int line;
  final int column;
  PopulationError(this.message, this.line, this.column);

  @override
  String toString() => '$message at line $line';
}

/// A module of a program with its name, as the checks need it.
class PopulationModule {
  final String name;
  final Module ast;
  PopulationModule(this.name, this.ast);
}

/// True if any module declares a kind or a run.
bool declaresPopulation(Iterable<PopulationModule> modules) =>
    modules.any((m) => m.ast.kinds.isNotEmpty || m.ast.runDecl != null);

/// The checks of the kinds and the run declaration of a program's [modules].
/// An empty list where all hold.
List<PopulationError> checkPopulation(Iterable<PopulationModule> modules) {
  final errors = <PopulationError>[];
  final kinds = <String, (KindDecl, PopulationModule)>{};
  RunDecl? run;
  PopulationModule? runModule;

  for (final m in modules) {
    for (final k in m.ast.kinds) {
      final prior = kinds[k.name];
      if (prior != null) {
        errors.add(PopulationError(
            'The kind "${k.name}" is declared twice, in ${prior.$2.name} and '
            'in ${m.name}',
            k.line,
            k.column));
        continue;
      }
      kinds[k.name] = (k, m);
    }
    final r = m.ast.runDecl;
    if (r != null) {
      if (run != null) {
        errors.add(PopulationError(
            'A second run declaration; the first is in ${runModule!.name}',
            r.line,
            r.column));
      } else {
        run = r;
        runModule = m;
      }
    }
  }

  // Each person declaration T =::= p names a procedure of its kind's program,
  // declared procedure p(T°, Integer?).
  final integerIn = TypeRef('Integer', 0, 0, isInput: true);
  for (final entry in kinds.values) {
    final (k, m) = entry;
    final seen = <String>{};
    for (final d in k.personDecls) {
      if (!seen.add(d.typeKey)) {
        errors.add(PopulationError(
            'The kind "${k.name}" declares the interactive type ${d.typeKey} '
            'twice',
            d.line,
            d.column));
      }
      if (!k.procedureSigs.contains('${d.procedure}/2')) {
        errors.add(PopulationError(
            'The person declaration ${d.typeKey} =::= ${d.procedure} of the '
            'kind "${k.name}" names no binary procedure of the kind\'s program',
            d.line,
            d.column));
        continue;
      }
      ProcDecl? decl;
      for (final pd in m.ast.procDeclarations) {
        if (pd.name == d.procedure && pd.arity == 2) decl = pd;
      }
      final dual = d.type.dual();
      final want = 'procedure ${d.procedure}($dual, Integer?)';
      if (decl == null) {
        errors.add(PopulationError(
            'The person procedure ${d.procedure} of ${d.typeKey} is not '
            'declared; it must be declared $want',
            d.line,
            d.column));
        continue;
      }
      if (decl.argTypes[0] != dual || decl.argTypes[1] != integerIn) {
        errors.add(PopulationError(
            'The person procedure ${d.procedure} of ${d.typeKey} is declared '
            'procedure ${d.procedure}(${decl.argTypes.join(', ')}); a person '
            'procedure of ${d.typeKey} is declared $want, its first argument '
            'the dual of ${d.typeKey}',
            decl.line,
            decl.column));
      }
    }
  }

  if (run == null) return errors;

  final dimNames = <String>{};
  for (final mix in run.mixes) {
    if (!dimNames.add(mix.dimension)) {
      errors.add(PopulationError(
          'The dimension "${mix.dimension}" is named twice in the run',
          mix.line,
          mix.column));
    }
    final inDim = <String>{};
    var sum = 0.0;
    Set<String>? types;
    String? firstKind;
    for (final e in mix.entries) {
      sum += e.probability;
      if (!(e.probability >= 0 && e.probability <= 1)) {
        errors.add(PopulationError(
            'The probability ${e.probability} of "${e.kind}" in '
            '"${mix.dimension}" is not in [0, 1]',
            e.line,
            e.column));
      }
      if (!inDim.add(e.kind)) {
        errors.add(PopulationError(
            'The kind "${e.kind}" is named twice in "${mix.dimension}"',
            e.line,
            e.column));
      }
      final k = kinds[e.kind];
      if (k == null) {
        errors.add(PopulationError(
            'The run names the kind "${e.kind}", which no "person ${e.kind}." '
            'declares',
            e.line,
            e.column));
        continue;
      }
      final t = k.$1.declaredTypes;
      if (types == null) {
        types = t;
        firstKind = e.kind;
      } else if (!_sameSet(types, t)) {
        errors.add(PopulationError(
            'The kinds of the dimension "${mix.dimension}" declare different '
            'interactive types: "$firstKind" declares ${_show(types)} and '
            '"${e.kind}" declares ${_show(t)}',
            e.line,
            e.column));
      }
    }
    if ((sum - 1).abs() > probabilityTolerance) {
      errors.add(PopulationError(
          'The probabilities of the dimension "${mix.dimension}" sum to $sum, '
          'not to one',
          mix.line,
          mix.column));
    }
  }
  return errors;
}

bool _sameSet(Set<String> a, Set<String> b) =>
    a.length == b.length && a.containsAll(b);

String _show(Set<String> s) =>
    s.isEmpty ? 'none' : (s.toList()..sort()).join(', ');

/// The population a program's [modules] declare, or null if they declare no
/// run.  The caller has run [checkPopulation] and found no error.
Population? populationOf(Iterable<PopulationModule> modules) {
  RunDecl? run;
  final kinds = <String, Kind>{};
  for (final m in modules) {
    run ??= m.ast.runDecl;
    for (final k in m.ast.kinds) {
      kinds[k.name] = Kind(k.name, m.name,
          {for (final d in k.personDecls) d.typeKey: d.procedure});
    }
  }
  if (run == null) return null;
  return Population(
    agents: run.agents,
    seed: run.seed,
    untilSeconds: run.untilSeconds,
    untilText: run.untilText,
    dimensions: [
      for (final mix in run.mixes)
        Dimension(mix.dimension, [for (final e in mix.entries) e.kind],
            [for (final e in mix.entries) e.probability])
    ],
    kinds: kinds,
  );
}
