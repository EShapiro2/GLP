import 'machine_state.dart';

/// Hanger: ensures single reactivation when a goal suspended on multiple readers.
class Hanger {
  final GoalId goalId;
  final Pc kappa;     // restart at clause selection
  bool armed;         // true at creation; first wake sets to false

  Hanger({required this.goalId, required this.kappa, this.armed = true});
}
