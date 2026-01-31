/// Minimal Variable Entry for multiagent runtime support
///
/// Provides VariableEntry for tracking suspensions on imported readers.
/// The full VariableTable (V_p) has been replaced by GlobalWritersTable (W_p)
/// for madGLP. This file provides only the entry type needed by the core
/// runtime for suspension management.
library;

import '../runtime/suspension.dart';
import '../runtime/terms.dart';

/// Entry for tracking variable state (suspensions and values)
///
/// Used by the core runtime to attach suspension lists to imported reader
/// cells that don't have a local writer cell.
class VariableEntry {
  /// Variable ID (local heap ID)
  final int varId;

  /// Whether this entry is for the reader view (true) or writer view (false)
  final bool isReader;

  /// Agent who created this variable
  final String creator;

  /// Creator's original local ID for this variable
  final int creatorLocalId;

  /// Cached bound value
  Term? boundValue;

  /// For imported writers: the creator's local ID of the paired reader.
  int? pairedReaderCreatorLocalId;

  /// Suspension list for goals waiting on this variable
  SuspensionListNode? suspensions;

  VariableEntry({
    required this.varId,
    required this.isReader,
    required this.creator,
    int? creatorLocalId,
    this.boundValue,
    this.pairedReaderCreatorLocalId,
  }) : creatorLocalId = creatorLocalId ?? varId;

  @override
  String toString() {
    final keyStr = isReader ? 'R$varId?' : 'W$varId';
    final creatorIdStr = creatorLocalId != varId ? ', creatorLocalId=$creatorLocalId' : '';
    return 'VarEntry($keyStr, creator=$creator$creatorIdStr)';
  }
}
