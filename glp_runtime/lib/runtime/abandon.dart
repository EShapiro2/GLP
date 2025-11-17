import 'machine_state.dart';

class AbandonOps {
  /// FCP-exact design: Abandon operation not yet implemented
  /// TODO: Implement FCP-compatible abandon semantics
  static List<GoalRef> abandonWriter({
    required int writerId,
  }) {
    throw UnimplementedError('Abandon operation not implemented in FCP design');
  }
}
