/// The declarations an agent has made to the networking layer and that stand: the record the store keeps of them, so that at a restart the runtime makes each to the new layer again (IGLP 07a2b00, Implementation Notes, "Persistence": the store keeps "every standing declaration --- the peers, the addresses, the rendezvous servers, the trust levels, the places, each with the centre it was declared about, and the nearby, reachable, public-address and network declarations"; "The networking layer keeps no declaration across a launch: at a restart, before the first goal is reduced, the runtime declares to the layer again every declaration the saved state holds").
///
/// Recorded here are the declarations a layer function receives: a peer, an address, a rendezvous server and a trust level, each where a layer is bound and took it, and a place, with its radius and, once the layer has answered, the centre the layer registered it about ([placeCentre]; IGLP 96cd44a, "Persistence": "the places with the centre each was declared about"; "at a restart the runtime makes every declaration the store holds to it again ..., each place about the centre the store holds for it"), whether a layer is bound or none is, since a place declaration stands either way ("One interface": "a place declaration records the place").  A place's stream and the number of its standing declaration are [GlpRuntime.placeStreams]'; a nearby or reachable declaration calls no layer function, and stands in its stream's bookkeeping alone ([GlpRuntime.nearbyStreams], [GlpRuntime.reachableStreams]), fed by what the new layer reports; a public-address or network declaration calls no layer function either, and is recorded here besides its stream's bookkeeping ([GlpRuntime.publicAddressStreams], [GlpRuntime.networkStreams]), so that a restart gives its standing stream the new layer's present value (the GLP paper f265fec, "Networking seam": each stream "beginning with" the present value "when it is declared").  A removal takes off what it removes: `remove(peer(P))` the peer and any address recorded for it (GLP-Spec appendix-guards, "Networking seam": "takes P, and any address recorded for it, off the layer's known peers"), `remove(rendezvous(P))` the server, `remove(place(P))` the place.  A further declaration of the same peer's address, the same server, the same underlay's trust level or the same place replaces the one standing and is put last, so the record holds the declarations in the order in which the standing ones were made, and a restart makes them to the new layer in that order.
library;

import 'saved_state.dart';

/// The kind of a standing declaration: the alternative of Declaration it was made by.
enum SeamDeclarationKind {
  peer,
  address,
  rendezvous,
  trust,
  place,
  publicAddress,
  network
}

/// A standing declaration: its [kind] and its arguments as the layer was given them --- a peer's key as 64 lowercase hex characters, an address, an underlay's and a level's names, a place's printed name and its radius printed, and then, where the layer has answered with the centre it registered the place about, its latitude and its longitude printed, in degrees.
class StandingDeclaration {
  final SeamDeclarationKind kind;
  final List<String> args;

  const StandingDeclaration(this.kind, this.args);

  @override
  String toString() => '${kind.name}(${args.join(', ')})';
}

class SeamDeclarations {
  /// The standing declarations, in the order they were made, by what a further declaration replaces: `peer:K`, `address:K`, `rendezvous:K`, `trust:U`, `place:P`, `public_address` and `network`.
  final Map<String, StandingDeclaration> _standing = {};

  /// The standing declarations, in the order they were made.
  Iterable<StandingDeclaration> get standing => _standing.values;

  void _put(String id, StandingDeclaration d) {
    _standing.remove(id);
    _standing[id] = d;
  }

  /// `declare(peer(P))`, the layer given the key of hex [key].
  void peer(String key) =>
      _put('peer:$key', StandingDeclaration(SeamDeclarationKind.peer, [key]));

  /// `remove(peer(P))`: the peer of hex [key] and any address recorded for it.
  void removePeer(String key) {
    _standing.remove('peer:$key');
    _standing.remove('address:$key');
  }

  /// `declare(address(P, A))`, the layer given [address] for the peer of hex [key].
  void address(String key, String address) => _put('address:$key',
      StandingDeclaration(SeamDeclarationKind.address, [key, address]));

  /// `declare(rendezvous(P, A))`, the layer given the server of hex [key] at [address].
  void rendezvous(String key, String address) => _put('rendezvous:$key',
      StandingDeclaration(SeamDeclarationKind.rendezvous, [key, address]));

  /// `remove(rendezvous(P))`.
  void removeRendezvous(String key) => _standing.remove('rendezvous:$key');

  /// `declare(trust(U, L))`, the underlay named [underlay] set to the level named [level].
  void trust(String underlay, String level) => _put('trust:$underlay',
      StandingDeclaration(SeamDeclarationKind.trust, [underlay, level]));

  /// `declare(place(P, R, E))`, the place of printed name [place] and radius [radius], about no centre yet: the layer answers with the centre it registered, which [placeCentre] then records.
  void place(String place, num radius) => _put('place:$place',
      StandingDeclaration(SeamDeclarationKind.place, [place, '$radius']));

  /// The centre the layer registered [place] about, [latitude] and [longitude] in degrees, kept with the place's standing declaration so that a restart declares the place again about it (IGLP 96cd44a, "Persistence").  Recorded in place: the declaration keeps its position in the order made, a centre being no further declaration.  Does nothing where no declaration of [place] stands, the place removed or declared again since.  Returns whether the record changed.
  bool placeCentre(String place, double latitude, double longitude) {
    final id = 'place:$place';
    final d = _standing[id];
    if (d == null) return false;
    final args = [d.args[0], d.args[1], '$latitude', '$longitude'];
    if (d.args.length == 4 && d.args[2] == args[2] && d.args[3] == args[3]) {
      return false;
    }
    _standing[id] = StandingDeclaration(SeamDeclarationKind.place, args);
    return true;
  }

  /// `remove(place(P))`.
  void removePlace(String place) => _standing.remove('place:$place');

  /// `declare(public_address(S))`: the one public-address declaration, a further one replacing it (the GLP paper f265fec, "Networking seam").
  void publicAddress() => _put('public_address',
      const StandingDeclaration(SeamDeclarationKind.publicAddress, []));

  /// `remove(public_address)`.
  void removePublicAddress() => _standing.remove('public_address');

  /// `declare(network(S))`: the one network declaration, a further one replacing it.
  void network() =>
      _put('network', const StandingDeclaration(SeamDeclarationKind.network, []));

  /// `remove(network)`.
  void removeNetwork() => _standing.remove('network');

  /// Write the standing declarations, in order.
  void saveState(StateWriter w) {
    w.uint(_standing.length);
    for (final MapEntry(key: id, value: d) in _standing.entries) {
      w.str(id);
      w.uint(d.kind.index);
      w.uint(d.args.length);
      for (final a in d.args) {
        w.str(a);
      }
    }
  }

  /// Read back what [saveState] wrote.
  void restoreState(StateReader r) {
    _standing.clear();
    for (var i = r.uint(); i > 0; i--) {
      final id = r.str();
      final kind = r.uint();
      if (kind >= SeamDeclarationKind.values.length) {
        throw SavedStateFormatException(
            'a standing declaration of kind $kind, which is no kind');
      }
      final args = [for (var j = r.uint(); j > 0; j--) r.str()];
      _standing[id] =
          StandingDeclaration(SeamDeclarationKind.values[kind], args);
    }
  }
}
