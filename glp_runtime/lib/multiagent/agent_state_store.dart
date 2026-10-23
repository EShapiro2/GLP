/// Where an agent's local state is kept across the termination of its
/// application (IGLP, Definition "Restart": "the local state s_p of p at
/// restart is its local state at termination"; Implementation Notes,
/// "Persistence": "The runtime keeps, beside the layer's queue, everything a
/// restarted agent runs from").
///
/// The embedding application gives the agent's host its store, as it gives
/// the layer the file of its queue: a file the application names, or, in the
/// simulation and the tests, the memory of the process that outlives the
/// agent.  The host saves the state at the end of each event cycle and reads
/// it at restart ([AgentRuntime]).
library;

import 'dart:io';
import 'dart:typed_data';

/// The store of one agent's saved state.
abstract class AgentStateStore {
  /// The state last saved, or null where none was.
  Future<Uint8List?> load();

  /// Save [state] in place of the state saved before it.  Completes once the
  /// state is kept; completes with an error where it is not, and then the
  /// state saved before it stands.
  Future<void> save(Uint8List state);
}

/// A store in one file, which the embedding application names.  A save writes
/// a file beside it and renames it over the file, so a termination during the
/// save leaves the state saved before it.
class FileAgentStateStore implements AgentStateStore {
  final String path;

  FileAgentStateStore(this.path);

  @override
  Future<Uint8List?> load() async {
    final f = File(path);
    if (!await f.exists()) return null;
    return f.readAsBytes();
  }

  @override
  Future<void> save(Uint8List state) async {
    final next = File('$path.next');
    await next.writeAsBytes(state, flush: true);
    await next.rename(path);
  }
}

/// A store in the memory of the process, which outlives the agents it hosts:
/// the simulation's and the tests'.  [failNextSaves] makes the next saves fail,
/// as a full disc does.
class MemoryAgentStateStore implements AgentStateStore {
  Uint8List? _state;

  /// The number of saves still to fail.
  int failNextSaves = 0;

  /// The number of saves kept.
  int saves = 0;

  @override
  Future<Uint8List?> load() async => _state;

  @override
  Future<void> save(Uint8List state) async {
    if (failNextSaves > 0) {
      failNextSaves--;
      throw const FileSystemException('the state was not kept');
    }
    _state = Uint8List.fromList(state);
    saves++;
  }
}
