import 'package:flutter/foundation.dart';
import '../models/peer.dart';
import '../transport/address_utils.dart';
import 'peers_state.dart';
import 'peers_actions.dart';

/// Reducer for peers-related state
PeersState peersReducer(PeersState state, dynamic action) {
  // ===== BLE Discovery Actions =====

  if (action is BleDeviceDiscoveredAction) {
    // TODO: why is nickname here? it comes in the announce
    final existing = state.discoveredBlePeers[action.deviceId];
    final now = DateTime.now();

    final DiscoveredPeerState newOrUpdated;
    if (existing == null) {
      newOrUpdated = DiscoveredPeerState(
        transportId: action.deviceId,
        displayName: action.displayName,
        rssi: action.rssi,
        serviceUuid: action.serviceUuid,
        discoveredAt: now,
        lastSeen: now,
      );
    } else {
      // TODO: this should be a different action, use update rssi
      newOrUpdated = existing.copyWith(
        rssi: action.rssi,
        serviceUuid: action.serviceUuid,
        lastSeen: now,
        displayName: (action.displayName?.isNotEmpty ?? false)
            ? action.displayName
            : existing.displayName,
      );
    }

    // Dedupe MAC-rotation ghost entries. Within one 15-minute slot the
    // advertised service UUID (Grassroots prefix + rotating per-slot suffix)
    // identifies the logical peer, while the deviceId / pathId is tied to the
    // radio MAC (Android) or CBPeripheral identifier (iOS without bonding),
    // which can rotate mid-slot. Without this cleanup the map accumulates one
    // dead entry per rotation, each one keeps getting re-dialed by
    // `_onAdvertisement`, and we end up in a status-133 storm. Across slot
    // boundaries the UUID itself rotates too, so this exact-match dedup no
    // longer fires — cross-slot ghosts are handled by the transport's
    // slot-candidate matching and swept by StaleDiscoveredBlePeersRemovedAction.
    //
    // Only prune entries that are NOT currently connected or in-flight: if
    // we still have a live or pending path on the old MAC, leave it alone —
    // the live-path guard in `_onAdvertisement` is what stops the new MAC
    // from racing a parallel dial.
    final updatedMap =
        Map<String, DiscoveredPeerState>.from(state.discoveredBlePeers);
    final newServiceUuid = action.serviceUuid?.toLowerCase();
    if (newServiceUuid != null && newServiceUuid.isNotEmpty) {
      updatedMap.removeWhere((deviceId, peer) =>
          deviceId != action.deviceId &&
          peer.serviceUuid?.toLowerCase() == newServiceUuid &&
          !peer.isConnected &&
          !peer.isConnecting);
    }
    updatedMap[action.deviceId] = newOrUpdated;

    return state.copyWith(discoveredBlePeers: updatedMap);
  }

  if (action is BleDeviceRssiUpdatedAction) {
    final existing = state.discoveredBlePeers[action.deviceId];
    if (existing != null) {
      final updated = existing.copyWith(
        rssi: action.rssi,
        lastSeen: DateTime.now(),
      );
      return state.copyWith(
        discoveredBlePeers: Map.from(state.discoveredBlePeers)
          ..[action.deviceId] = updated,
      );
    }
    return state;
  }

  if (action is BleDeviceConnectingAction) {
    final existing = state.discoveredBlePeers[action.deviceId];
    if (existing != null) {
      final updated = existing.copyWith(isConnecting: true);
      return state.copyWith(
        discoveredBlePeers: Map.from(state.discoveredBlePeers)
          ..[action.deviceId] = updated,
      );
    }
    return state;
  }

  if (action is BleDeviceConnectedAction) {
    final existing = state.discoveredBlePeers[action.deviceId];
    if (existing != null) {
      final updated = existing.copyWith(isConnecting: false, isConnected: true);
      return state.copyWith(
        discoveredBlePeers: Map.from(state.discoveredBlePeers)
          ..[action.deviceId] = updated,
      );
    }
    return state;
  }

  if (action is BleDeviceConnectionFailedAction) {
    final existing = state.discoveredBlePeers[action.deviceId];
    if (existing != null) {
      final updated = existing.copyWith(
        isConnecting: false,
        isConnected: false,
      );
      return state.copyWith(
        discoveredBlePeers: Map.from(state.discoveredBlePeers)
          ..[action.deviceId] = updated,
      );
    }
    return state;
  }

  if (action is BleDeviceDisconnectedAction) {
    final existing = state.discoveredBlePeers[action.deviceId];
    if (existing != null) {
      final updated = existing.copyWith(
        isConnecting: false,
        isConnected: false,
      );
      return state.copyWith(
        discoveredBlePeers: Map.from(state.discoveredBlePeers)
          ..[action.deviceId] = updated,
      );
    }
    return state;
  }

  if (action is BleDeviceRemovedAction) {
    final newMap = Map<String, DiscoveredPeerState>.from(
      state.discoveredBlePeers,
    );
    newMap.remove(action.deviceId);
    return state.copyWith(discoveredBlePeers: newMap);
  }

  if (action is StaleDiscoveredBlePeersRemovedAction) {
    final now = DateTime.now();
    final newMap = Map<String, DiscoveredPeerState>.from(
      state.discoveredBlePeers,
    );
    newMap.removeWhere((_, peer) {
      final timeSinceLastSeen = now.difference(peer.lastSeen);
      return timeSinceLastSeen > action.staleThreshold;
    });
    return state.copyWith(discoveredBlePeers: newMap);
  }

  if (action is ClearDiscoveredBlePeersAction) {
    return state.copyWith(discoveredBlePeers: {});
  }

  // ===== Peer Identity Actions =====

  if (action is PeerAnnounceReceivedAction) {
    final pubkeyHex = _pubkeyToHex(action.publicKey);
    final existing = state.peers[pubkeyHex];
    final now = DateTime.now();
    final actionCandidates = normalizeAddressStrings([
      action.linkLocalAddress,
      action.udpAddress,
      ...action.udpAddressCandidates,
    ]);

    final isBle = action.transport == PeerTransport.bleDirect;

    if (existing == null) {
      // New peer — no prior reachability evidence.
      final newPeer = PeerState(
        publicKey: action.publicKey,
        nickname: action.nickname,
        connectionState: PeerConnectionState.connected,
        transport: action.transport,
        rssi: action.rssi,
        protocolVersion: action.protocolVersion,
        lastSeen: now,
        bleCentralDeviceId: action.bleCentralDeviceId,
        blePeripheralDeviceId: action.blePeripheralDeviceId,
        lastBleSeen: isBle ? now : null,
        lastUdpSeen: action.transport == PeerTransport.udp ? now : null,
        udpAddress: action.udpAddress,
        linkLocalAddress: action.linkLocalAddress,
        udpAddressCandidates: actionCandidates,
      );
      return state.copyWith(
        peers: Map.from(state.peers)..[pubkeyHex] = newPeer,
      );
    } else {
      // Update existing peer.
      // Merge BLE IDs: only update the field that's provided in this action,
      // preserve the other from existing state.
      //
      // TODO: Revert to unconditional `udpAddress: action.udpAddress` once
      // the BLE layer sends EITHER a friend ANNOUNCE (with address) OR a
      // non-friend ANNOUNCE (without address) per recipient — never both.
      // Currently a friend receives both because the peripheral can't
      // reliably determine which centrals are friends (BLE device ID
      // rotation). The non-friend broadcast (no address) arrives and nukes
      // the address set by the UDP ANNOUNCE, causing peers to flicker in
      // the online friends list. This null-coalescing is a workaround.
      //
      // TODO: Fix BLE peripheral to reliably map central device IDs to
      // friend public keys so it can skip friends in the broadcast and
      // only send them the directed friend ANNOUNCE with address.
      final newUdpAddress = action.udpAddress ?? existing.udpAddress;
      final newUdpAddressCandidates = actionCandidates.isNotEmpty
          ? actionCandidates
          : existing.udpAddressCandidates;
      // If the UDP address changed, prior reachability observation is stale
      // because it was bound to the previous address/network path.
      final preserveReach = newUdpAddress == existing.udpAddress &&
          setEquals(newUdpAddressCandidates, existing.udpAddressCandidates);
      // RSSI: copyWith already treats null as "no update" (rssi ?? this.rssi),
      // so a UDP ANNOUNCE (action.rssi == null) keeps any existing BLE-derived
      // RSSI. A BLE ANNOUNCE carries the per-packet payload.rssi and overwrites.
      final updated = existing.copyWith(
        nickname: action.nickname,
        connectionState: PeerConnectionState.connected,
        transport: action.transport,
        rssi: action.rssi,
        protocolVersion: action.protocolVersion,
        lastSeen: now,
        bleCentralDeviceId:
            action.bleCentralDeviceId ?? existing.bleCentralDeviceId,
        blePeripheralDeviceId:
            action.blePeripheralDeviceId ?? existing.blePeripheralDeviceId,
        lastBleSeen: isBle ? now : existing.lastBleSeen,
        lastUdpSeen:
            action.transport == PeerTransport.udp ? now : existing.lastUdpSeen,
        udpAddress: newUdpAddress,
        linkLocalAddress: action.linkLocalAddress ?? existing.linkLocalAddress,
        udpAddressCandidates: newUdpAddressCandidates,
        clearLastDirectReachAt: !preserveReach,
      );
      return state.copyWith(
        peers: Map.from(state.peers)..[pubkeyHex] = updated,
      );
    }
  }

  if (action is PeerRssiUpdatedAction) {
    final pubkeyHex = _pubkeyToHex(action.publicKey);
    final existing = state.peers[pubkeyHex];
    if (existing != null) {
      final updated = existing.copyWith(
        rssi: action.rssi,
        lastSeen: DateTime.now(),
      );
      return state.copyWith(
        peers: Map.from(state.peers)..[pubkeyHex] = updated,
      );
    }
    return state;
  }

  if (action is PeerBleDisconnectedAction) {
    final pubkeyHex = _pubkeyToHex(action.publicKey);
    final existing = state.peers[pubkeyHex];
    if (existing != null) {
      // Determine which BLE IDs to clear based on role
      final clearCentral =
          action.role == null || action.role == BleRole.central;
      final clearPeripheral =
          action.role == null || action.role == BleRole.peripheral;

      final newCentralId = clearCentral ? null : existing.bleCentralDeviceId;
      final newPeripheralId =
          clearPeripheral ? null : existing.blePeripheralDeviceId;
      final hasAnyBle = newCentralId != null || newPeripheralId != null;

      // A BLE-disconnect event describes a BLE fact. Deliberately do NOT
      // consult `hasLiveUdpConnection` here: mixing transport-orthogonal
      // state into the projection muddles two unrelated UI dimensions
      // ("Connected Peers" cares about BLE; "Friends Online" cares about
      // UDP). Once both BLE roles are gone the peer's `connectionState`
      // flips to `disconnected`; UDP-derived state is updated separately
      // by `PeerUdpDisconnectedAction` / `PeerUdpConnectionChangedAction`.
      final newConnectionState =
          hasAnyBle ? existing.connectionState : PeerConnectionState.disconnected;

      // Construct directly to allow clearing nullable fields.
      // RSSI is meaningful only while a BLE link exists; clear it when the
      // last BLE path drops so the peer doesn't carry a stale dBm value.
      final updated = PeerState(
        publicKey: existing.publicKey,
        nickname: existing.nickname,
        connectionState: newConnectionState,
        transport: existing.transport,
        rssi: hasAnyBle ? existing.rssi : null,
        protocolVersion: existing.protocolVersion,
        lastSeen: existing.lastSeen,
        bleCentralDeviceId: newCentralId,
        blePeripheralDeviceId: newPeripheralId,
        lastBleSeen: hasAnyBle ? existing.lastBleSeen : null,
        lastUdpSeen: existing.lastUdpSeen,
        udpAddress: existing.udpAddress,
        linkLocalAddress: existing.linkLocalAddress,
        udpAddressCandidates: existing.udpAddressCandidates,
        isFriend: existing.isFriend,
        lastDirectReachAt: existing.lastDirectReachAt,
        hasLiveUdpConnection: existing.hasLiveUdpConnection,
        // Clear BLE auth only when the last BLE path is gone; a partial drop
        // (one role) leaves the Noise session intact.
        bleAuthenticated: hasAnyBle ? existing.bleAuthenticated : false,
      );
      return state.copyWith(
        peers: Map.from(state.peers)..[pubkeyHex] = updated,
      );
    }
    return state;
  }

  if (action is PeerBleAuthenticatedAction) {
    final pubkeyHex = _pubkeyToHex(action.publicKey);
    final existing = state.peers[pubkeyHex];
    if (existing != null) {
      return state.copyWith(
        peers: Map.from(state.peers)
          ..[pubkeyHex] = existing.copyWith(bleAuthenticated: true),
      );
    }
    return state;
  }

  if (action is PeerUdpConnectionChangedAction) {
    final existing = state.peers[action.pubkeyHex];
    if (existing != null) {
      final updated = existing.copyWith(hasLiveUdpConnection: action.connected);
      return state.copyWith(
        peers: Map.from(state.peers)..[action.pubkeyHex] = updated,
      );
    }
    return state;
  }

  if (action is PeerUdpSeenAction) {
    final pubkeyHex = _pubkeyToHex(action.publicKey);
    final existing = state.peers[pubkeyHex];
    if (existing != null) {
      final now = DateTime.now();
      final updated = existing.copyWith(lastSeen: now, lastUdpSeen: now);
      return state.copyWith(
        peers: Map.from(state.peers)..[pubkeyHex] = updated,
      );
    }
    return state;
  }

  if (action is PeerUdpDisconnectedAction) {
    final pubkeyHex = _pubkeyToHex(action.publicKey);
    final existing = state.peers[pubkeyHex];
    if (existing != null) {
      // If no other transport, mark as disconnected
      final newConnectionState = existing.hasBleConnection
          ? existing.connectionState
          : PeerConnectionState.disconnected;
      // Preserve UDP address — it's the last known location and needed
      // for reconnection. Never clear peer addresses.
      // Clear hasLiveUdpConnection — transport-level disconnect means no live stream.
      final updated = PeerState(
        publicKey: existing.publicKey,
        nickname: existing.nickname,
        connectionState: newConnectionState,
        transport: existing.transport,
        rssi: existing.rssi,
        protocolVersion: existing.protocolVersion,
        lastSeen: existing.lastSeen,
        bleCentralDeviceId: existing.bleCentralDeviceId,
        blePeripheralDeviceId: existing.blePeripheralDeviceId,
        lastBleSeen: existing.lastBleSeen,
        lastUdpSeen: existing.lastUdpSeen,
        udpAddress: existing.udpAddress, // Preserve for reconnection
        linkLocalAddress: existing.linkLocalAddress,
        udpAddressCandidates: existing.udpAddressCandidates,
        isFriend: existing.isFriend,
        lastDirectReachAt: existing.lastDirectReachAt,
        hasLiveUdpConnection: false,
        // Transport independence: a UDP drop must not touch BLE auth.
        bleAuthenticated: existing.bleAuthenticated,
      );
      return state.copyWith(
        peers: Map.from(state.peers)..[pubkeyHex] = updated,
      );
    }
    return state;
  }

  if (action is PeerDisconnectedAction) {
    final pubkeyHex = _pubkeyToHex(action.publicKey);
    final existing = state.peers[pubkeyHex];
    if (existing != null) {
      final updated = existing.copyWith(
        connectionState: PeerConnectionState.disconnected,
      );
      return state.copyWith(
        peers: Map.from(state.peers)..[pubkeyHex] = updated,
      );
    }
    return state;
  }

  if (action is PeerRemovedAction) {
    final pubkeyHex = _pubkeyToHex(action.publicKey);
    final newMap = Map<String, PeerState>.from(state.peers);
    newMap.remove(pubkeyHex);
    return state.copyWith(peers: newMap);
  }

  if (action is StalePeersRemovedAction) {
    // Memory pressure: forget non-friend peers we haven't heard from within
    // [staleThreshold]. We do NOT mutate `connectionState` here — that field
    // is exclusively driven by plugin events (BLE) and UDX events (UDP), so
    // that both sides of a connection see the same transitions. Friends are
    // kept regardless so we can reconnect to them later.
    final now = DateTime.now();
    final newMap = Map<String, PeerState>.from(state.peers);
    final staleKeys = <String>[];
    newMap.forEach((key, peer) {
      if (peer.isFriend) return;
      if (peer.lastSeen == null) return;
      final timeSinceLastSeen = now.difference(peer.lastSeen!);
      if (timeSinceLastSeen > action.staleThreshold) {
        staleKeys.add(key);
      }
    });
    for (final key in staleKeys) {
      newMap.remove(key);
    }
    return state.copyWith(peers: newMap);
  }

  if (action is ClearAllPeersAction) {
    return state.copyWith(peers: {});
  }

  // ===== Association Actions =====

  if (action is AssociateBleDeviceAction) {
    final pubkeyHex = _pubkeyToHex(action.publicKey);
    final existing = state.peers[pubkeyHex];
    if (existing != null) {
      final updated = action.role == BleRole.central
          ? existing.copyWith(bleCentralDeviceId: action.deviceId)
          : existing.copyWith(blePeripheralDeviceId: action.deviceId);
      return state.copyWith(
        peers: Map.from(state.peers)..[pubkeyHex] = updated,
      );
    }
    return state;
  }

  if (action is AssociateUdpAddressAction) {
    final pubkeyHex = _pubkeyToHex(action.publicKey);
    final existing = state.peers[pubkeyHex];
    if (existing != null) {
      final newAddress = action.address.isEmpty ? null : action.address;
      final newCandidates = newAddress == null
          ? const <String>{}
          : normalizeAddressStrings([
              existing.linkLocalAddress,
              newAddress,
              ...existing.udpAddressCandidates,
            ]);
      // If the address changed, prior reachability observation is stale.
      final preserveReach = newAddress == existing.udpAddress &&
          setEquals(newCandidates, existing.udpAddressCandidates);
      final updated = PeerState(
        publicKey: existing.publicKey,
        nickname: existing.nickname,
        connectionState: existing.connectionState,
        transport: existing.transport,
        rssi: existing.rssi,
        protocolVersion: existing.protocolVersion,
        lastSeen: existing.lastSeen,
        bleCentralDeviceId: existing.bleCentralDeviceId,
        blePeripheralDeviceId: existing.blePeripheralDeviceId,
        lastBleSeen: existing.lastBleSeen,
        lastUdpSeen: existing.lastUdpSeen,
        udpAddress: newAddress,
        linkLocalAddress: existing.linkLocalAddress,
        udpAddressCandidates: newCandidates,
        isFriend: existing.isFriend,
        lastDirectReachAt: preserveReach ? existing.lastDirectReachAt : null,
        hasLiveUdpConnection: existing.hasLiveUdpConnection,
        bleAuthenticated: existing.bleAuthenticated,
      );
      return state.copyWith(
        peers: Map.from(state.peers)..[pubkeyHex] = updated,
      );
    }
    // Unknown peer: GLP fed an address (via putPeerAddress) for a peer we have
    // not yet met. Materialize a minimal dial-book entry so send and
    // reconnection can reach it. An empty address has nothing to store.
    final newAddress = action.address.isEmpty ? null : action.address;
    if (newAddress == null) return state;
    final created = PeerState(
      publicKey: action.publicKey,
      nickname: '',
      connectionState: PeerConnectionState.disconnected,
      transport: PeerTransport.udp,
      udpAddress: newAddress,
      udpAddressCandidates: normalizeAddressStrings([newAddress]),
    );
    return state.copyWith(
      peers: Map.from(state.peers)..[pubkeyHex] = created,
    );
  }

  if (action is PeerIdentityRegisteredAction) {
    final pubkeyHex = _pubkeyToHex(action.publicKey);
    if (state.peers.containsKey(pubkeyHex)) return state;
    // Identity learned out-of-band (cold-call invite): a minimal disconnected
    // entry with no address — reachability and addresses arrive later via
    // sessions, ANNOUNCE, or putPeerAddress.
    final created = PeerState(
      publicKey: action.publicKey,
      nickname: '',
      connectionState: PeerConnectionState.disconnected,
      transport: PeerTransport.udp,
    );
    return state.copyWith(
      peers: Map.from(state.peers)..[pubkeyHex] = created,
    );
  }

  if (action is PeerFriendListUpdatedAction) {
    final pubkeyHex = _pubkeyToHex(action.publicKey);
    final existing = state.peers[pubkeyHex];
    if (existing == null || !existing.isFriend) return state;

    final normalized = action.friendPubkeyHexes
        .map((hex) => hex.toLowerCase())
        .where((hex) => hex.length == 64 && hex != pubkeyHex)
        .toSet();
    final current = state.friendsOfFriends[pubkeyHex] ?? const <String>{};
    if (setEquals(current, normalized)) return state;

    return state.copyWith(
      friendsOfFriends: Map<String, Set<String>>.from(state.friendsOfFriends)
        ..[pubkeyHex] = normalized,
    );
  }

  // ===== Friendship Actions =====

  if (action is FriendEstablishedAction) {
    final pubkeyHex = _pubkeyToHex(action.publicKey);
    final existing = state.peers[pubkeyHex];

    if (existing != null) {
      final updated = existing.copyWith(
        isFriend: true,
        nickname: action.nickname ?? existing.nickname,
      );
      return state.copyWith(
        peers: Map.from(state.peers)..[pubkeyHex] = updated,
      );
    } else {
      // Peer may not exist yet (e.g. hydrated from FriendshipStore on startup)
      final newPeer = PeerState(
        publicKey: action.publicKey,
        nickname: action.nickname ?? '',
        connectionState: PeerConnectionState.discovered,
        lastSeen: DateTime.now(),
        isFriend: true,
      );
      return state.copyWith(
        peers: Map.from(state.peers)..[pubkeyHex] = newPeer,
      );
    }
  }

  if (action is FriendRemovedAction) {
    final pubkeyHex = _pubkeyToHex(action.publicKey);
    final existing = state.peers[pubkeyHex];
    if (existing != null) {
      final friendsOfFriends = Map<String, Set<String>>.from(
        state.friendsOfFriends,
      )..remove(pubkeyHex);
      friendsOfFriends.updateAll(
        (_, friendSet) => Set<String>.from(friendSet)..remove(pubkeyHex),
      );
      // If peer has no BLE connection, remove them entirely
      // (they were only reachable via UDP friendship)
      if (!existing.hasBleConnection) {
        final newMap = Map<String, PeerState>.from(state.peers);
        newMap.remove(pubkeyHex);
        return state.copyWith(
          peers: newMap,
          friendsOfFriends: friendsOfFriends,
        );
      }

      // Peer is still nearby via BLE - keep them but clear friend status
      final updated = PeerState(
        publicKey: existing.publicKey,
        nickname: existing.nickname,
        connectionState: PeerConnectionState.connected,
        transport: PeerTransport.bleDirect, // Reset to BLE
        rssi: existing.rssi,
        protocolVersion: existing.protocolVersion,
        lastSeen: existing.lastSeen,
        bleCentralDeviceId: existing.bleCentralDeviceId,
        blePeripheralDeviceId: existing.blePeripheralDeviceId,
        lastBleSeen: existing.lastBleSeen,
        lastUdpSeen: existing.lastUdpSeen,
        // Clear all UDP/friend fields
        isFriend: false,
        udpAddress: null,
        udpAddressCandidates: const {},
        lastDirectReachAt: null,
        // Still nearby over BLE — keep the BLE auth state.
        bleAuthenticated: existing.bleAuthenticated,
      );
      return state.copyWith(
        peers: Map.from(state.peers)..[pubkeyHex] = updated,
        friendsOfFriends: friendsOfFriends,
      );
    }
    return state;
  }

  if (action is PeerDirectReachObservedAction) {
    final pubkeyHex = _pubkeyToHex(action.publicKey);
    final existing = state.peers[pubkeyHex];
    if (existing == null) return state;
    // Only meaningful if the peer has a public-looking address.
    if (!existing.hasPublicUdpAddress) return state;
    final updated = existing.copyWith(lastDirectReachAt: action.observedAt);
    return state.copyWith(peers: Map.from(state.peers)..[pubkeyHex] = updated);
  }

  return state;
}

String _pubkeyToHex(List<int> pubkey) {
  return pubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
}
