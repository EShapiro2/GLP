/// Where an agent's local state is kept across the termination of its
/// application (IGLP, Definition "Restart": "the local state s_p of p at
/// restart is its local state at termination"; Implementation Notes,
/// "Persistence": "In the meantime the runtime keeps it by a store the host
/// gives it, beside the layer's queue").
///
/// The embedding application gives the agent's host its store, as it gives
/// the layer the file of its queue: a file the application names, or, in the
/// simulation and the tests, the memory of the process that outlives the
/// agent.  The host saves the state when the application goes to the
/// background, which the platform announces before it may kill it, and at no
/// other time ([AgentRuntime.save]; IGLP d70ec2e, "Persistence"), and reads
/// it at restart ([AgentRuntime]).
///
/// The store keeps the agent's key pair too, first among what Persistence
/// lists: the state is first saved when the application first goes to the
/// background, and the key pair is needed before it, the layer being
/// instantiated with it and the modules compiled for the agent certified
/// under it (Implementation Notes, "Boot"), so the store keeps it apart from
/// the state, and [keptKeyPair] makes it once, at the first start.
///
/// The store keeps the compiled program the agent runs beside the key pair
/// (IGLP 61f3283, "Persistence": "the agent's key pair, the compiled program
/// it runs, the resolvent ..."; [StoredProgram]): a restart may load a new
/// version of the program, and where the update is refused the agent
/// restarts under the version it terminated under (Termination and Restart),
/// which the store holds.  The program is kept apart from the state too, as
/// it changes only at an update, which the first save under it then keeps.
library;

import 'dart:io';
import 'dart:typed_data';

import 'glp_network.dart' show PubKey;
import 'identity.dart';

/// An agent's Ed25519 key pair: its public key, and the 64-byte private key
/// of `ed25519_edwards`, the 32-byte seed followed by the public key.
typedef AgentKeyPair = ({PubKey pub, Uint8List priv});

/// The key pair [store] keeps for the agent of persona [persona], made and
/// saved at the agent's first start: "it makes the agent's key pair from a
/// seed drawn from outside the program --- the SHA-256 of the clock, the
/// device's hardware identifiers and the persona's name ... --- and keeps it
/// across restarts (Persistence), so the key pair is made once, at the
/// agent's first start" (IGLP, Implementation Notes, "Boot").  Throws where
/// the key pair cannot be kept: an agent whose key is not kept would have
/// another at its next start.
Future<AgentKeyPair> keptKeyPair(AgentStateStore store, String persona) async {
  final kept = await store.loadKeyPair();
  if (kept != null) return kept;
  final made = PersonIdentity.forPersona(persona).keyPair;
  await store.saveKeyPair(made);
  return made;
}

/// The store of one agent's saved state and key pair.
abstract class AgentStateStore {
  /// The state last saved, or null where none was.
  Future<Uint8List?> load();

  /// Save [state] in place of the state saved before it.  Completes once the
  /// state is kept; completes with an error where it is not, and then the
  /// state saved before it stands.
  Future<void> save(Uint8List state);

  /// The agent's key pair, or null where none was saved: before its first
  /// start.
  Future<AgentKeyPair?> loadKeyPair();

  /// Save the agent's [keyPair].  Completes once it is kept; completes with
  /// an error where it is not.
  Future<void> saveKeyPair(AgentKeyPair keyPair);

  /// The compiled program the agent runs, as last saved ([StoredProgram]'s
  /// bytes), or null where none was.
  Future<Uint8List?> loadProgram();

  /// Save [program] in place of the program saved before it.  Completes once
  /// it is kept; completes with an error where it is not, and then the
  /// program saved before it stands.
  Future<void> saveProgram(Uint8List program);

  /// Whether the running mark is set: "At every start and at every
  /// resumption the runtime sets a mark, running, in the store, and the save
  /// clears it" (IGLP db0a117, Implementation Notes, "Persistence", When).
  /// Set at a start, the application crashed since it last ran: it neither
  /// saved at a suspension nor terminated from one ("Restart": "If the
  /// running mark is set at a start, the application crashed").  False where
  /// the store is new.
  Future<bool> loadRunning();

  /// Set the running mark, or clear it.  Completes once the mark is kept;
  /// completes with an error where it is not.
  Future<void> saveRunning(bool running);
}

/// A store in one file, which the embedding application names, the key
/// pair's file beside it, `<path>.key`, holding the 32-byte seed from which
/// the pair is made again, and the program's, `<path>.program`.  A save writes
/// a file beside it and renames it over the file, so a termination during the
/// save leaves the file saved before it.
class FileAgentStateStore implements AgentStateStore {
  final String path;

  FileAgentStateStore(this.path);

  File get _keyFile => File('$path.key');

  File get _programFile => File('$path.program');

  /// The running mark: the file `<path>.running`, which exists while the
  /// mark is set.
  File get _runningFile => File('$path.running');

  @override
  Future<bool> loadRunning() => _runningFile.exists();

  @override
  Future<void> saveRunning(bool running) async {
    final f = _runningFile;
    if (running) {
      await f.parent.create(recursive: true);
      await f.writeAsString('running', flush: true);
    } else if (await f.exists()) {
      await f.delete();
    }
  }

  @override
  Future<Uint8List?> loadProgram() async {
    final f = _programFile;
    if (!await f.exists()) return null;
    return f.readAsBytes();
  }

  @override
  Future<void> saveProgram(Uint8List program) async {
    final f = _programFile;
    await f.parent.create(recursive: true);
    final next = File('${f.path}.next');
    await next.writeAsBytes(program, flush: true);
    await next.rename(f.path);
  }

  @override
  Future<Uint8List?> load() async {
    final f = File(path);
    if (!await f.exists()) return null;
    return f.readAsBytes();
  }

  @override
  Future<void> save(Uint8List state) async {
    final next = File('$path.next');
    // The directory the application names may not exist before the agent's
    // first save, as on a phone at its first launch.
    await next.parent.create(recursive: true);
    await next.writeAsBytes(state, flush: true);
    await next.rename(path);
  }

  @override
  Future<AgentKeyPair?> loadKeyPair() async {
    final f = _keyFile;
    if (!await f.exists()) return null;
    final seed = await f.readAsBytes();
    if (seed.length != 32) {
      throw StateError('${f.path} holds ${seed.length} bytes, and an '
          'agent\'s key pair is kept as its 32-byte seed');
    }
    return PersonIdentity.fromSeed(seed).keyPair;
  }

  @override
  Future<void> saveKeyPair(AgentKeyPair keyPair) async {
    await _keyFile.parent.create(recursive: true);
    final next = File('${_keyFile.path}.next');
    await next.writeAsBytes(Uint8List.sublistView(keyPair.priv, 0, 32),
        flush: true);
    await next.rename(_keyFile.path);
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

  AgentKeyPair? _keyPair;

  Uint8List? _program;

  /// The number of programs saved.
  int programSaves = 0;

  /// The running mark ([loadRunning]).
  bool running = false;

  @override
  Future<bool> loadRunning() async => running;

  @override
  Future<void> saveRunning(bool running) async => this.running = running;

  @override
  Future<Uint8List?> loadProgram() async => _program;

  @override
  Future<void> saveProgram(Uint8List program) async {
    _program = Uint8List.fromList(program);
    programSaves++;
  }

  @override
  Future<AgentKeyPair?> loadKeyPair() async => _keyPair;

  @override
  Future<void> saveKeyPair(AgentKeyPair keyPair) async => _keyPair = keyPair;

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
