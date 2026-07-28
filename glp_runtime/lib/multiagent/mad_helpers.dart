/// Helper types and operations for madGLP
///
/// Provides Globalize and Localize operations as specified in
/// madGLP-spec.md Sections 5.1 and 5.2.
///
/// These operations transform terms between local and global representations
/// for inter-agent communication.
library;

import 'package:glp_runtime/runtime/terms.dart';
import 'global_writers_table.dart';
import 'imported_writer_records.dart';

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

/// A variable reference in a term (for globalize/localize)
///
/// Represents either a writer (addr) or reader (addr with isReader=true).
/// Always carries both writer and reader addresses of the pair.
class TermVar {
  final int addr;
  final bool isReader;

  /// Writer address of the variable pair
  final int writerAddr;

  /// Reader address of the variable pair
  final int readerAddr;

  /// Create a writer variable reference
  TermVar.writer(this.addr, {required this.readerAddr})
      : isReader = false,
        writerAddr = addr;

  /// Create a reader variable reference
  TermVar.reader(this.addr, {required this.writerAddr})
      : isReader = true,
        readerAddr = addr;

  bool get isWriter => !isReader;

  /// Get the paired reader address
  int get pairedReaderAddr => readerAddr;

  @override
  String toString() => isReader
      ? 'TermVar.reader($addr, writer=$writerAddr)'
      : 'TermVar.writer($addr, reader=$readerAddr)';
}

/// Result of a Globalize operation
///
/// See: madGLP-spec.md Section 5.1
class GlobalizeResult {
  /// Global names substituted for variables, in order of occurrence
  final List<GlobalName> globalNames;

  /// Spawns needed for reader variables
  final List<GlobalSendSpawn> spawns;

  /// Reader names forwarded under their original anchor (Definition Globalize,
  /// case 3): the LocalizeEntry was removed and no goal spawned.
  /// The caller records these so a later value arriving for the name is
  /// dropped as stale, not held as early.
  final List<GlobalName> forwardedNames;

  /// Imported writers forwarded under their original anchor (Definition
  /// Globalize, case 4): the record was removed and the original name `_w(o,k)`
  /// emitted. The caller removes each record's `global_send` goal — the goal is
  /// registered under the record's writer address — so the exporting agent
  /// leaves the link instead of relaying its values.
  final List<ImportedWriterRecord> forwardedWriters;

  GlobalizeResult({
    required this.globalNames,
    required this.spawns,
    this.forwardedNames = const [],
    this.forwardedWriters = const [],
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
  /// false = use writer (Y_q) for _w names, true = use reader (Z_q?) for _r names
  final List<bool> useReader;

  /// Spawns needed for _w(p,i) global names
  final List<GlobalSendSpawn> spawns;

  /// Requests to queue (Definition Localize, request rule): for each _r(p, i)
  /// received from a sender s ≠ p the name was forwarded, and req(_r(p,i)) is
  /// sent to the anchor p — the request is how the anchor learns the reader's
  /// current holder. Each name here is addressed to its own anchor (gn.agent).
  final List<GlobalName> requests;

  LocalizeResult({
    required this.freshPairs,
    required this.useReader,
    required this.spawns,
    this.requests = const [],
  });
}

/// Globalize operation
///
/// Given agent p, remote agent q, and a list of variables occurring in term T,
/// produces the globalized representation T_p↑.
///
/// Definition Globalize (IGLP, madglp.tex):
/// - Case 1. If Y is a writer: allocate index i, create entry (Y, q) at index
///   i, replace with _w(p, i). No goal is spawned.
/// - Case 2. If Y? is a reader: allocate index i, replace with _r(p, i),
///   spawn global_send(Y?, _r(p,i), q). No entry is created.
/// - Case 3. If Y? is the reader of an imported pair — its paired writer
///   carries a LocalizeEntry (Y, o, k) with anchor o — replace with the
///   original name _r(o, k) and remove the entry: the name is forwarded, no
///   goal is spawned.
/// - Case 4. If Y is the writer of an imported pair — [records] has the record
///   (Y, o, k) with anchor o — replace with the original name _w(o, k), remove
///   the record and the global_send goal watching Y?, and create no entry: the
///   name is forwarded, and p leaves the link.
///
/// Returns GlobalizeResult with global names, spawn info, forwarded reader
/// names, and the forwarded imported-writer records whose goals the caller
/// must remove. Updates the GlobalWritersTable and the imported-writer records.
GlobalizeResult globalize({
  required List<TermVar> variables,
  required String localAgent,
  required String remoteAgent,
  required GlobalWritersTable table,
  required ImportedWriterRecords records,
}) {
  final globalNames = <GlobalName>[];
  final spawns = <GlobalSendSpawn>[];
  final forwardedNames = <GlobalName>[];
  final forwardedWriters = <ImportedWriterRecord>[];

  for (final v in variables) {
    if (v.isWriter) {
      // Case 4 — writer of an imported pair: forward under the original name.
      // Spec: "replace Y in T_p↑ with the original name _w(o, k), remove the
      // record and the global_send goal watching Y?, and create no entry: the
      // name is forwarded, and p leaves the link." The record's presence is
      // the condition; it is removed when the value is sent, so a writer whose
      // value has already gone carries none.
      final importedWriter = records.remove(v.writerAddr);
      if (importedWriter != null) {
        globalNames.add(
            GlobalName.writer(importedWriter.anchor, importedWriter.index));
        forwardedWriters.add(importedWriter);
        continue;
      }

      // Case 1 — writer: create entry, no spawn
      // Spec: "allocate the next index i, create entry (Y, q) at index i
      //        in W'_p, and replace Y in T_p↑ with _w(p, i).
      //        No goal is spawned—p will receive the assignment on this link."
      // Entry stores the writer address Y for later binding when value arrives.
      final index = table.addGlobalizeEntry(v.writerAddr, remoteAgent);
      globalNames.add(GlobalName.writer(localAgent, index));
      continue;
    }

    // Case 3 — reader of an imported pair: forward under the original name.
    // Spec: "replace Y? with the original name _r(o, k) and remove the entry:
    // the name is forwarded, and no goal is spawned." The entry's presence
    // implies the reader is unbound — applying the incoming value removes it.
    final imported = table.findLocalizeEntryByWriter(v.writerAddr);
    if (imported != null) {
      final original =
          GlobalName.reader(imported.remoteAgent, imported.remoteIndex);
      table.removeLocalizeEntry(imported.remoteAgent, imported.remoteIndex);
      globalNames.add(original);
      forwardedNames.add(original);
      continue;
    }

    // Case 2 — reader: spawn global_send, no entry
    // Spec: "allocate the next index i, replace Y? in T_p↑ with _r(p, i),
    //        and spawn global_send(Y?, _r(p,i), q). No entry is created—
    //        the global_send goal handles outgoing communication."
    final index = table.allocateIndex();
    final globalName = GlobalName.reader(localAgent, index);
    globalNames.add(globalName);

    // Spawn global_send(Y?, _r(p,i), q)
    // Note: GlobalSendSpawn.readerAddr is used as the key for heap.onBind(),
    // which is indexed by *writer* address. We pass writerAddr so the
    // callback fires when bindVariable is called on Y.
    spawns.add(GlobalSendSpawn(
      readerAddr: v.writerAddr,
      globalName: globalName,
      destAgent: remoteAgent,
    ));
  }

  return GlobalizeResult(
    globalNames: globalNames,
    spawns: spawns,
    forwardedNames: forwardedNames,
    forwardedWriters: forwardedWriters,
  );
}

/// Localize operation
///
/// Given agent q, remote agent p, and a list of global names from term T_p↑,
/// produces the localized representation T_q↓.
///
/// Definition Localize (IGLP, madglp.tex):
/// - If _w(p, i): create fresh pair (Y_q, Y_q?), replace with Y_q (writer),
///   add the record (Y_q, p, i) to U'_q, and spawn global_send(Y_q?, _w(p,i),
///   p). No entry is created.
/// - If _r(p, i): create fresh pair (Z_q, Z_q?), add entry (Z_q, p, i),
///   replace with Z_q? (reader). No goal is spawned. If the sending agent
///   [fromAgent] is not p, the name was forwarded, and req(_r(p,i)) is added
///   to the result's requests, addressed to the anchor p.
///
/// Returns LocalizeResult with fresh pairs, usage info, spawn info, and
/// requests. Updates the GlobalWritersTable with entries for _r names and the
/// imported-writer records [records] with records for _w names.
///
/// The [freshAddrAllocator] function is called to allocate each fresh variable pair,
/// returning (writerAddr, readerAddr).
LocalizeResult localize({
  required List<GlobalName> globalNames,
  required String localAgent,
  required String fromAgent,
  required GlobalWritersTable table,
  required ImportedWriterRecords records,
  required (int, int) Function() freshAddrAllocator,
}) {
  final freshPairs = <FreshPair>[];
  final useReader = <bool>[];
  final spawns = <GlobalSendSpawn>[];
  final requests = <GlobalName>[];

  for (final gn in globalNames) {
    // Allocate fresh pair
    final (writerAddr, readerAddr) = freshAddrAllocator();
    final pair = FreshPair(writerAddr, readerAddr);
    freshPairs.add(pair);

    if (gn.isWriter) {
      // _w(p, i): record the imported writer, spawn global_send, use writer
      // Spec: "create fresh local pair (Y_q, Y_q?), replace _w(p, i) with
      //        Y_q (the writer) in T_q↓, add the record (Y_q, p, i) to U'_q,
      //        and spawn global_send(Y_q?, _w(p,i), p)."
      //        No entry is created—the global_send goal handles outgoing communication.
      useReader.add(false); // Use Y_q (writer)

      // The record marks Y_q as the sending end of the link anchored at
      // _w(p, i): re-exporting Y_q forwards that name (Globalize case 4)
      // instead of anchoring a new link here. The anchor is gn.agent whether
      // or not the sender is the anchor — a forwarded writer keeps its name.
      records.add(writerAddr, gn.agent, gn.index);

      // Spawn global_send(Y_q?, _w(p,i), p)
      // When q assigns Y_q, Y_q? becomes known, gs fires and sends value to p.
      // Note: GlobalSendSpawn.readerAddr is used as the key for heap.onBind(),
      // which is indexed by *writer* address. We pass writerAddr so the callback
      // fires when bindVariable(writerAddr, ...) is called.
      spawns.add(GlobalSendSpawn(
        readerAddr: writerAddr,
        globalName: gn,
        destAgent: gn.agent, // Send back to agent p who created the name
      ));
    } else {
      // _r(p, i): create entry, use reader
      // Spec: "create fresh local pair (Z_q, Z_q?), allocate the next index k
      //        in W'_q, add entry (Z_q, p, i), and replace _r(p, i) with Z_q?
      //        (the reader) in T_q↓. No goal is spawned—q will receive the
      //        assignment on this link."
      table.addLocalizeEntry(writerAddr, gn.agent, gn.index);
      useReader.add(true); // Use Z_q? (reader)

      // Request rule: "If the sending agent is not p, the name was forwarded,
      // and q adds the request (req(_r(p,i)), p) to its outgoing messages —
      // the request is how the anchor p learns the reader's current holder."
      if (fromAgent != gn.agent) {
        requests.add(gn);
      }
    }
  }

  return LocalizeResult(
    freshPairs: freshPairs,
    useReader: useReader,
    spawns: spawns,
    requests: requests,
  );
}

// ============================================================================
// Term Transformation Functions
// ============================================================================

/// Transform a term by replacing variables with their global names
///
/// Takes the original term and the GlobalizeResult from globalize().
/// Returns a new term with VarRefs replaced by StructTerms representing
/// global names (_w(agent, index) or _r(agent, index)).
Term globalizeTermWithResult(
  Term term,
  List<TermVar> variables,
  GlobalizeResult result,
) {
  final varToGlobalName = <int, GlobalName>{};
  for (var i = 0; i < variables.length; i++) {
    varToGlobalName[variables[i].addr] = result.globalNames[i];
  }
  return _substituteGlobalNames(term, varToGlobalName);
}

Term _substituteGlobalNames(Term term, Map<int, GlobalName> mapping) {
  if (term is VarRef) {
    final gn = mapping[term.addr];
    if (gn != null) {
      final functor = gn.isWriter ? '_w' : '_r';
      return StructTerm(functor, [ConstTerm(gn.agent), ConstTerm(gn.index)]);
    }
    return term;
  } else if (term is StructTerm) {
    final newArgs = term.args.map((a) => _substituteGlobalNames(a, mapping)).toList();
    return StructTerm(term.functor, newArgs);
  }
  return term; // ConstTerm unchanged
}

/// Extract global name structures from a term
///
/// Finds all _w(agent, index) and _r(agent, index) structures in the term.
/// Returns the list of GlobalNames in order of occurrence.
List<GlobalName> extractGlobalNames(Term term) {
  final result = <GlobalName>[];
  _extractGlobalNamesRecursive(term, result);
  return result;
}

void _extractGlobalNamesRecursive(Term term, List<GlobalName> result) {
  if (term is StructTerm) {
    if ((term.functor == '_w' || term.functor == '_r') && term.args.length == 2) {
      final agentArg = term.args[0];
      final indexArg = term.args[1];
      if (agentArg is ConstTerm && indexArg is ConstTerm) {
        final agent = agentArg.value as String;
        final index = (indexArg.value as num).toInt();
        result.add(term.functor == '_w'
            ? GlobalName.writer(agent, index)
            : GlobalName.reader(agent, index));
      }
    } else {
      for (final arg in term.args) {
        _extractGlobalNamesRecursive(arg, result);
      }
    }
  }
}

/// Transform a term by replacing global names with local variables
///
/// Takes the globalized term and the LocalizeResult from localize().
/// Returns a new term with global name structures replaced by VarRefs.
Term localizeTermWithResult(
  Term term,
  List<GlobalName> globalNames,
  LocalizeResult result,
) {
  final globalNameToLocal = <String, int>{};
  for (var i = 0; i < globalNames.length; i++) {
    final gn = globalNames[i];
    final pair = result.freshPairs[i];
    final useReader = result.useReader[i];
    globalNameToLocal['${gn.type.name}:${gn.agent}:${gn.index}'] =
        useReader ? pair.readerAddr : pair.writerAddr;
  }
  return _substituteLocalVars(term, globalNameToLocal);
}

Term _substituteLocalVars(Term term, Map<String, int> mapping) {
  if (term is StructTerm) {
    if ((term.functor == '_w' || term.functor == '_r') && term.args.length == 2) {
      final agentArg = term.args[0];
      final indexArg = term.args[1];
      if (agentArg is ConstTerm && indexArg is ConstTerm) {
        final type = term.functor == '_w' ? 'writer' : 'reader';
        final key = '$type:${agentArg.value}:${indexArg.value}';
        final localAddr = mapping[key];
        if (localAddr != null) {
          return VarRef(localAddr);
        }
      }
    }
    final newArgs = term.args.map((a) => _substituteLocalVars(a, mapping)).toList();
    return StructTerm(term.functor, newArgs);
  }
  return term;
}
