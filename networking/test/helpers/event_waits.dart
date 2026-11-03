import 'dart:async';
import 'dart:io';

import 'package:redux/redux.dart';

import 'package:grassroots_networking_core/src/store/store.dart';

/// Waits on the event a test tests, with no clock (networking/CLAUDE.md,
/// rule 4: "A test waits on the event it tests, a `Completer` the callback
/// completes, with `timeout: Timeout.none`; no clock, no poll and no deadline
/// decides a test").

/// Completes once [holds] is true of [store]'s state: at once if it holds
/// now, and otherwise at the first change of state after which it holds.
/// The store is the projection of the facts the transports emit, so each
/// change is the event.
Future<void> stateWhere(
    Store<AppState> store, bool Function(AppState state) holds) async {
  if (holds(store.state)) return;
  await store.onChange.firstWhere(holds);
}

/// A hermetic host-address reader, loopback alone, that tells when the layer
/// holds the addresses it read: [read] completes on the turn of the event
/// loop after the reader returns, once the layer's continuation has recorded
/// them.
class LoopbackHostReader {
  final Completer<void> _read = Completer<void>();

  Future<List<InternetAddress>> call() async {
    Timer.run(() {
      if (!_read.isCompleted) _read.complete();
    });
    return [InternetAddress.loopbackIPv4];
  }

  /// Completes once the layer holds its host candidates.
  Future<void> get read => _read.future;
}
