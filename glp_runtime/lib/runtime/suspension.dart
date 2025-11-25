/// Shared Suspension Records (FCP Design)
/// One SuspensionRecord shared across multiple lists via wrapper nodes
/// Activated once, then disarmed to prevent double-activation
library;

/// Shared suspension state (one per suspended goal)
/// Multiple SuspensionListNodes can point to the same record
class SuspensionRecord {
  int? goalId;        // Process ID - nullable for disarming
  final int resumePC; // Where to resume (kappa - procedure entry point)
  final String moduleName;  // Module this goal executes in

  SuspensionRecord(this.goalId, this.resumePC, this.moduleName);

  /// Disarm this record (prevent future activation)
  void disarm() {
    goalId = null;
  }

  /// Check if this record is armed (can activate)
  bool get armed => goalId != null;

  @override
  String toString() => 'SuspensionRecord(goal=$goalId, pc=$resumePC, module=$moduleName, armed=$armed)';
}

/// Wrapper node for linking shared records into suspension lists
/// Each reader cell stores a SuspensionListNode with independent 'next' pointers
/// Multiple nodes can point to the same SuspensionRecord (shared state)
class SuspensionListNode {
  final SuspensionRecord record;  // Points to shared record
  SuspensionListNode? next;       // List chain (independent per reader)

  SuspensionListNode(this.record);

  /// Delegate to shared record
  bool get armed => record.armed;
  int? get goalId => record.goalId;
  int get resumePC => record.resumePC;
  String get moduleName => record.moduleName;

  @override
  String toString() => 'SuspensionListNode(record=$record)';
}
