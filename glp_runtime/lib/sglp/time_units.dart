// glp_runtime/lib/sglp/time_units.dart
//
// sGLP's units of simulated time (svGLP, sections/sglp.tex, Syntax):
//
//   <rate>      ::= <positive_real> / <time_unit>
//   <time_unit> ::= second | minute | hour | day | week | year
//
// The simulated clock is kept in seconds: a rate r / u is r / seconds(u) per
// second, and the `until <time>` of a run declaration is a number of seconds.
// A year is the Julian year of 365.25 days; the paper names the unit and not
// its length (an implementation decision, for svGLP's Implementation Notes).

/// Seconds in one of each time unit of the grammar.
const Map<String, double> secondsPerUnit = {
  'second': 1,
  'minute': 60,
  'hour': 3600,
  'day': 86400,
  'week': 604800,
  'year': 31557600,
};

/// The seconds in [unit], a unit of the grammar, or null if it is not one.
///
/// A rate's unit is singular, as the grammar writes it (`1/week`).  A time,
/// `until 5 years`, may also be plural, as the paper's run declaration writes
/// it: [allowPlural] admits `seconds` ... `years`.
double? secondsOfUnit(String unit, {bool allowPlural = false}) {
  final s = secondsPerUnit[unit];
  if (s != null) return s;
  if (allowPlural && unit.endsWith('s')) {
    return secondsPerUnit[unit.substring(0, unit.length - 1)];
  }
  return null;
}

/// The unit names, for diagnostics.
String get timeUnitNames => secondsPerUnit.keys.join(', ');
