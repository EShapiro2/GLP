/// Helper types and operations for madGLP
///
/// Provides Globalize and Localize operations as specified in
/// madGLP-spec.md Sections 5.1 and 5.2.
///
/// These operations transform terms between local and global representations
/// for inter-agent communication.
library;

import 'global_writers_table.dart';

/// Type of global variable name
enum GlobalNameType {
  /// `_w(p, i)` - writer globalized at p
  writer,

  /// `_r(p, i)` - reader globalized at p
  reader,
}

/// Global variable name: `_w(p, i)` or `_r(p, i)`
///
/// See: madGLP-spec.md Section 2
class GlobalName {
  final GlobalNameType type;
  final String agent;
  final int index;

  GlobalName(this.type, this.agent, this.index);

  /// Create a writer global name `_w(agent, index)`
  GlobalName.writer(this.agent, this.index) : type = GlobalNameType.writer;

  /// Create a reader global name `_r(agent, index)`
  GlobalName.reader(this.agent, this.index) : type = GlobalNameType.reader;

  bool get isWriter => type == GlobalNameType.writer;
  bool get isReader => type == GlobalNameType.reader;

  @override
  String toString() =>
      type == GlobalNameType.writer ? '_w($agent, $index)' : '_r($agent, $index)';

  @override
  bool operator ==(Object other) =>
      other is GlobalName &&
      type == other.type &&
      agent == other.agent &&
      index == other.index;

  @override
  int get hashCode => Object.hash(type, agent, index);
}

/// Information needed to spawn a global_send goal
///
/// Represents the goal: `global_send(readerAddr, globalName, destAgent)`
///
/// See: madGLP-spec.md Section 4
class GlobalSendSpawn {
  /// Address of the reader to watch (the ? end of the variable pair)
  final int readerAddr;

  /// Global name identifying the link
  final GlobalName globalName;

  /// Destination agent
  final String destAgent;

  GlobalSendSpawn({
    required this.readerAddr,
    required this.globalName,
    required this.destAgent,
  });

  @override
  String toString() =>
      'GlobalSendSpawn(reader=$readerAddr, name=$globalName, dest=$destAgent)';
}

/// A variable reference in a term (for globalize/localize testing)
///
/// Represents either a writer (addr) or reader (addr with isReader=true)
class TermVar {
  final int addr;
  final bool isReader;

  /// Create a writer variable reference
  TermVar.writer(this.addr) : isReader = false;

  /// Create a reader variable reference
  TermVar.reader(this.addr) : isReader = true;

  bool get isWriter => !isReader;

  /// Get the paired reader address (same address, different mode)
  int get pairedReaderAddr => addr;

  @override
  String toString() => isReader ? 'TermVar.reader($addr)' : 'TermVar.writer($addr)';
}

/// Result of a Globalize operation
///
/// See: madGLP-spec.md Section 5.1
class GlobalizeResult {
  /// Global names substituted for variables, in order of occurrence
  final List<GlobalName> globalNames;

  /// Spawns needed for writer variables
  final List<GlobalSendSpawn> spawns;

  GlobalizeResult({
    required this.globalNames,
    required this.spawns,
  });
}

/// A fresh variable pair created during localization
class FreshPair {
  final int writerAddr;
  final int readerAddr;

  FreshPair(this.writerAddr, this.readerAddr);

  @override
  String toString() => 'FreshPair(writer=$writerAddr, reader=$readerAddr)';
}

/// Result of a Localize operation
///
/// See: madGLP-spec.md Section 5.2
class LocalizeResult {
  /// Fresh variable pairs created, one per global name
  final List<FreshPair> freshPairs;

  /// What to substitute in the term: writer addr or reader addr per position
  /// true = use reader (Y_q?), false = use writer (Z_q)
  final List<bool> useReader;

  /// Spawns needed for _r(p,i) global names
  final List<GlobalSendSpawn> spawns;

  LocalizeResult({
    required this.freshPairs,
    required this.useReader,
    required this.spawns,
  });
}

/// Globalize operation
///
/// Given agent p, remote agent q, and a list of variables occurring in term T,
/// produces the globalized representation T_p↑.
///
/// Spec Section 5.1:
/// - If Y is a writer: allocate index i, replace with _w(p, i), spawn global_send(Y?, _w(p,i), q)
/// - If Y? is a reader: allocate index i, create entry (Y, q) at index i, replace with _r(p, i)
///
/// Returns GlobalizeResult with global names and spawn info.
/// Updates the GlobalWritersTable with entries for readers.
GlobalizeResult globalize({
  required List<TermVar> variables,
  required String localAgent,
  required String remoteAgent,
  required GlobalWritersTable table,
}) {
  final globalNames = <GlobalName>[];
  final spawns = <GlobalSendSpawn>[];

  for (final v in variables) {
    if (v.isWriter) {
      // Writer: spawn global_send, no entry
      // Spec: "allocate the next index i, replace Y with _w(p, i),
      //        spawn global_send(Y?, _w(p,i), q). No entry is created."
      final index = table.allocateIndex();
      final globalName = GlobalName.writer(localAgent, index);
      globalNames.add(globalName);

      // Spawn global_send(Y?, _w(p,i), q)
      spawns.add(GlobalSendSpawn(
        readerAddr: v.pairedReaderAddr,
        globalName: globalName,
        destAgent: remoteAgent,
      ));
    } else {
      // Reader: create entry, no spawn
      // Spec: "allocate the next index i, create entry (Y, q) at index i,
      //        replace Y? with _r(p, i). No goal is spawned."
      final index = table.addGlobalizeEntry(v.addr, remoteAgent);
      globalNames.add(GlobalName.reader(localAgent, index));
    }
  }

  return GlobalizeResult(globalNames: globalNames, spawns: spawns);
}

/// Localize operation
///
/// Given agent q, remote agent p, and a list of global names from term T_p↑,
/// produces the localized representation T_q↓.
///
/// Spec Section 5.2:
/// - If _w(p, i): create fresh pair, add entry (Y_q, p, i), use Y_q? (reader)
/// - If _r(p, i): create fresh pair, spawn global_send(Z_q?, _r(p,i), p), use Z_q (writer)
///
/// Returns LocalizeResult with fresh pairs, usage info, and spawn info.
/// Updates the GlobalWritersTable with entries for _w names.
///
/// The [freshAddrAllocator] function is called to allocate each fresh writer address.
LocalizeResult localize({
  required List<GlobalName> globalNames,
  required String localAgent,
  required GlobalWritersTable table,
  required int Function() freshAddrAllocator,
}) {
  final freshPairs = <FreshPair>[];
  final useReader = <bool>[];
  final spawns = <GlobalSendSpawn>[];

  for (final gn in globalNames) {
    // Allocate fresh pair
    final writerAddr = freshAddrAllocator();
    final readerAddr = writerAddr; // Reader uses same address in our model
    final pair = FreshPair(writerAddr, readerAddr);
    freshPairs.add(pair);

    if (gn.isWriter) {
      // _w(p, i): create entry with remote index, use reader
      // Spec: "create fresh local pair (Y_q, Y_q?), allocate the next index k,
      //        add entry (Y_q, p, i), replace _w(p, i) with Y_q? (the reader)"
      table.addLocalizeEntry(writerAddr, gn.agent, gn.index);
      useReader.add(true); // Use Y_q?
    } else {
      // _r(p, i): spawn global_send, use writer
      // Spec: "create fresh local pair (Z_q, Z_q?), replace _r(p, i) with Z_q,
      //        spawn global_send(Z_q?, _r(p,i), p)"
      useReader.add(false); // Use Z_q

      // Spawn global_send(Z_q?, _r(p,i), p)
      spawns.add(GlobalSendSpawn(
        readerAddr: readerAddr,
        globalName: gn,
        destAgent: gn.agent, // Send back to the agent who created the name
      ));
    }
  }

  return LocalizeResult(
    freshPairs: freshPairs,
    useReader: useReader,
    spawns: spawns,
  );
}
