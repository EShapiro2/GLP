/// Shared Suspension Records (FCP Design)
/// Same record appears in multiple variables' suspension lists
/// Activated once, then disarmed to prevent double-activation
library;

class SuspensionRecord {
  int? goalId;        // Process ID - nullable for disarming
  final int resumePC; // Where to resume (kappa - procedure entry point)
  SuspensionRecord? next; // Next record in linked list

  SuspensionRecord(this.goalId, this.resumePC, {this.next});

  /// Disarm this record (prevent future activation)
  void disarm() {
    goalId = null;
  }

  /// Check if this record is armed (can activate)
  bool get armed => goalId != null;

  @override
  String toString() => 'SuspensionRecord(goal=$goalId, pc=$resumePC, armed=$armed)';
}
