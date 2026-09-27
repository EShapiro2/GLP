// glp_runtime/lib/sglp/draws.dart
//
// The engine's draws in an sGLP run (svGLP, sections/sglp.tex): the
// exponential delay of each rated goal at its spawn (Definition "sGLP
// Transition System", Machine), each agent's kind per dimension (Definition
// "Dual, Person Procedure, ..., Population"), and the seed handed to a person
// goal (Definition "Simulation Program").
//
// Every draw is a pure function of the run's seed and of identifiers that do
// not depend on the machine's schedule: a draw is a hash of the seed and those
// identifiers, never the next number of a generator shared by the run.  So two
// runs with one seed draw the same numbers whatever order the machine takes
// its steps in, and the draws are independent as the paper requires, up to the
// quality of the hash.  The hash is SplitMix64's finaliser (Steele, Lea and
// Flood, "Fast splittable pseudorandom number generators", OOPSLA 2014).

import 'dart:math' as math;

/// Tags keeping the families of draws apart.
const int tagDelay = 0x5d1a7e;   // the exponential delay of a rated goal
const int tagKind = 0x6b1d;      // an agent's kind in a dimension
const int tagPerson = 0x9e450;   // the seed handed to a person goal
const int tagLineage = 0x11e46e; // a goal's identifier from its parent's
const int tagRoot = 0x2007;      // the identifier of a posted (initial) goal

/// SplitMix64's finaliser: a bijective 64-bit mix with full avalanche.
int mix64(int z) {
  z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9;
  z = (z ^ (z >>> 27)) * 0x94d049bb133111eb;
  return z ^ (z >>> 31);
}

/// Combine a hash with one more value.
int combine(int h, int v) => mix64(h ^ mix64(v + 0x9e3779b97f4a7c15));

/// Combine a hash with several values, in order.
int combineAll(int h, Iterable<int> vs) {
  var r = h;
  for (final v in vs) {
    r = combine(r, v);
  }
  return r;
}

/// A uniform real in (0, 1] from the top 53 bits of [h].
double unitOpenClosed(int h) => ((h >>> 11) + 1) * (1.0 / 9007199254740992.0);

/// A uniform real in [0, 1) from the top 53 bits of [h].
double unitClosedOpen(int h) => (h >>> 11) * (1.0 / 9007199254740992.0);

/// The identifier of a goal spawned as the [ordinal]-th goal of the body of
/// the reduction of the goal identified by [parent].  A goal is reduced at
/// most once, so (parent, ordinal) names one goal of the run, and the name
/// depends on the goal's ancestry alone, not on when the machine reduced it.
int childLineage(int parent, int ordinal) =>
    combine(combine(parent, tagLineage), ordinal);

/// The identifier of the [index]-th goal posted to the machine from outside
/// (an initial goal of a run).
int rootLineage(int index) => combine(tagRoot, index);

/// The exponential delay, with rate [ratePerSecond], of the rated goal
/// identified by [lineage] in the run seeded by [seed].
double exponentialDelay(int seed, int lineage, double ratePerSecond) {
  final u = unitOpenClosed(combine(combine(seed, tagDelay), lineage));
  return -math.log(u) / ratePerSecond;
}

/// The seed a person goal is handed (Definition "Simulation Program"):
/// derived from the population's [seed], the agent [agent] and the
/// identifier [askedGoal] of the asked goal.  It lies in [1, 2^31 - 2], the
/// range GLP's `random/4` normalises its seed into.
int personSeed(int seed, int agent, int askedGoal) {
  final h = combineAll(combine(seed, tagPerson), [agent, askedGoal]);
  return 1 + (h >>> 1) % 2147483646;
}

/// A uniform real in [0, 1) for [agent]'s kind in dimension [dimension].
double kindDraw(int seed, int agent, int dimension) =>
    unitClosedOpen(combineAll(combine(seed, tagKind), [agent, dimension]));
