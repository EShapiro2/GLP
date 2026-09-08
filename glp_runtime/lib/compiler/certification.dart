/// The OS-privileged refusal (SGSG, Section 3 "What the super-app grants a
/// mini-app" and Section 6.1 G1; IGLP code format §Program Artefact): the
/// compiler certifies no program that calls a system predicate or kernel
/// reaching the network or the person, so a certified module reaches them only
/// through variables its host gives it.
///
/// The decision is by reachability in the flat, pruned program — the linked
/// program restricted to the procedures reachable from its exported entry
/// points — and not by names in source text, so that a wrapper does not pass:
/// a procedure whose body names a privileged predicate or kernel is itself
/// privileged, and so is any procedure that calls it. The root scope's own
/// clauses (root `self.glp`, the madGLP system predicates) are closed over the
/// same way, once, so a root-scope predicate such as `send_to_user/1`, whose
/// clauses are not part of any flat program, is refused by the name the flat
/// program calls it by.
library;

import 'ast.dart';
import 'lexer.dart';
import 'parser.dart';

/// The body kernels that reach the network or the person (IGLP Cowork,
/// 2026-09-08; GLP-Spec appendix-guards, the Network and I/O kernel rows).
/// `'_trust_declare'` is in the catalogue and not registered; it is named here
/// so that it is refused once it exists.
const Set<String> privilegedKernels = {
  '_send/3',
  '_output/1',
  '_peer_address/2',
  '_punch_udp/1',
  '_place_declare/3',
  '_place_remove/1',
  '_trust_declare/2',
  '_authorise_link/2',
};

/// The system predicates named as OS-privileged (SGSG's request; the
/// catalogue's seam rows), by the names the flat program calls them by. The
/// closure over the root clauses below finds these again from the kernels; they
/// are listed so the refusal is by specification and not only by the clauses
/// that happen to stand in the root today.
const Set<String> privilegedPredicates = {
  'send_to_net/1',
  'send_to_user/1',
  'send_to_person/1',
  'peer_address/2',
  'punch_udp/1',
  'place_declare/3',
  'place_remove/1',
  'trust_declare/2',
  'authorise_link/2',
};

/// Every goal a clause body reaches, through spawn wrappers and residual
/// dynamic remote goals.
Iterable<Goal> _bodyGoals(Clause c) sync* {
  for (final g in c.body ?? const <Goal>[]) {
    yield* _flatten(g);
  }
}

Iterable<Goal> _flatten(Goal g) sync* {
  if (g is SpawnGoal) {
    yield* _flatten(g.innerGoal);
  } else if (g is RemoteGoal) {
    yield* _flatten(g.goal);
  } else {
    yield g;
  }
}

/// The signatures of [procedures] from which a signature in [seed] is
/// reachable: the least set containing [seed] and every procedure whose body
/// names a member of it.
Set<String> _closure(List<Procedure> procedures, Set<String> seed) {
  final privileged = <String>{...seed};
  var grew = true;
  while (grew) {
    grew = false;
    for (final p in procedures) {
      final sig = '${p.name}/${p.arity}';
      if (privileged.contains(sig)) continue;
      for (final c in p.clauses) {
        if (_bodyGoals(c)
            .any((g) => privileged.contains('${g.functor}/${g.arity}'))) {
          privileged.add(sig);
          grew = true;
          break;
        }
      }
    }
  }
  return privileged;
}

/// The privileged names of the root scope: the kernels, the predicates named
/// above, and every procedure of [rootSources] (root `self.glp`, the madGLP
/// system predicates) from which one of them is reachable. Computed once per
/// engine.
Set<String> privilegedRootNames(Iterable<String> rootSources) {
  final procedures = <Procedure>[];
  for (final src in rootSources) {
    final module = Parser(Lexer(src).tokenize()).parseModule();
    procedures.addAll(module.procedures);
  }
  return _closure(procedures, {...privilegedKernels, ...privilegedPredicates});
}

/// An offending call: [caller] names [callee], which reaches the network or
/// the person.
class PrivilegedCall {
  final String caller;
  final String callee;
  const PrivilegedCall(this.caller, this.callee);

  @override
  String toString() => '$caller calls $callee';
}

/// The calls for which [program] — a flat, pruned program — is refused a
/// certificate. The decision is by reachability: the program is refused where
/// any of its procedures reaches, through any chain of calls, a privileged
/// root name. The calls NAMED are the boundary crossings — a body goal of the
/// program's own code ([ownModules], by the `M:` prefix the linker gave each
/// procedure; bare names are the program's own too) that names something
/// privileged outside it: a kernel, a root-scope predicate, or a procedure of
/// an exposed system module — so that the line names the call in the
/// mini-app's source and not the chain beneath it. Empty where the program is
/// certifiable.
List<PrivilegedCall> privilegedCalls(
    Program program, Set<String> privilegedRoot,
    {required Set<String> ownModules}) {
  final privileged = _closure(program.procedures, privilegedRoot);
  bool own(String name) {
    final colon = name.lastIndexOf(':');
    return colon < 0 || ownModules.contains(name.substring(0, colon));
  }

  // A boundary callee: a privileged root name (a kernel, or a root-scope
  // predicate, which the flat program calls bare), or a privileged procedure
  // of a module that is not the program's own.
  bool boundary(String callee, String functor) =>
      privileged.contains(callee) &&
      (privilegedRoot.contains(callee) || !own(functor));

  final offending = <PrivilegedCall>[];
  for (final p in program.procedures) {
    if (!own(p.name)) continue;
    final caller = '${p.name}/${p.arity}';
    final seen = <String>{};
    for (final c in p.clauses) {
      for (final g in _bodyGoals(c)) {
        final callee = '${g.functor}/${g.arity}';
        if (boundary(callee, g.functor) && seen.add(callee)) {
          offending.add(PrivilegedCall(caller, callee));
        }
      }
    }
  }
  // A program that reaches a privileged name only through its own code — every
  // boundary crossing lies in an exposed module it calls into — is still
  // refused; name the first own call into the privileged set.
  if (offending.isEmpty) {
    for (final p in program.procedures) {
      if (!own(p.name)) continue;
      final caller = '${p.name}/${p.arity}';
      for (final c in p.clauses) {
        for (final g in _bodyGoals(c)) {
          final callee = '${g.functor}/${g.arity}';
          if (privileged.contains(callee)) {
            offending.add(PrivilegedCall(caller, callee));
            return offending;
          }
        }
      }
    }
  }
  return offending;
}
