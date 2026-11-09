import '../models/peer.dart';
import '../models/underlay.dart';
import 'peers_state.dart';
import 'peers_actions.dart';

/// Reducer for peers-related state
PeersState peersReducer(PeersState state, dynamic action) {
  // ===== BLE Discovery Actions =====

  if (action is BleDeviceDiscoveredAction) {
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

    final isBle = action.transport == PeerTransport.bleDirect;

    if (existing == null) {
      // New peer — identity beacon only; addresses arrive via putPeerAddress.
      final newPeer = PeerState(
        publicKey: action.publicKey,
        connectionState: PeerConnectionState.connected,
        transport: action.transport,
        rssi: action.rssi,
        protocolVersion: action.protocolVersion,
        lastSeen: now,
        bleCentralDeviceId: action.bleCentralDeviceId,
        blePeripheralDeviceId: action.blePeripheralDeviceId,
        lastBleSeen: isBle ? now : null,
        lastLanSeen: action.transport == PeerTransport.lan ? now : null,
        lastWanSeen: action.transport == PeerTransport.wan ? now : null,
      );
      return state.copyWith(
        peers: Map.from(state.peers)..[pubkeyHex] = newPeer,
      );
    } else {
      // Update existing peer.
      // Merge BLE IDs: only update the field that's provided in this action,
      // preserve the other from existing state.
      // RSSI: copyWith already treats null as "no update" (rssi ?? this.rssi),
      // so a UDP ANNOUNCE (action.rssi == null) keeps any existing BLE-derived
      // RSSI. A BLE ANNOUNCE carries the per-packet payload.rssi and overwrites.
      final updated = existing.copyWith(
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
        lastLanSeen:
            action.transport == PeerTransport.lan ? now : existing.lastLanSeen,
        lastWanSeen:
            action.transport == PeerTransport.wan ? now : existing.lastWanSeen,
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
      // consult the IP sessions here: mixing transport-orthogonal
      // state into the projection muddles two unrelated UI dimensions
      // ("Connected Peers" cares about BLE; "Friends Online" cares about
      // UDP). Once both BLE roles are gone the peer's `connectionState`
      // flips to `disconnected`; UDP-derived state is updated separately
      // by `PeerIpDisconnectedAction` / `PeerIpSessionChangedAction`.
      final newConnectionState =
          hasAnyBle ? existing.connectionState : PeerConnectionState.disconnected;

      // Construct directly to allow clearing nullable fields.
      // RSSI is meaningful only while a BLE link exists; clear it when the
      // last BLE path drops so the peer doesn't carry a stale dBm value.
      final updated = PeerState(
        publicKey: existing.publicKey,
        connectionState: newConnectionState,
        transport: existing.transport,
        rssi: hasAnyBle ? existing.rssi : null,
        protocolVersion: existing.protocolVersion,
        lastSeen: existing.lastSeen,
        bleCentralDeviceId: newCentralId,
        blePeripheralDeviceId: newPeripheralId,
        lastBleSeen: hasAnyBle ? existing.lastBleSeen : null,
        lastLanSeen: existing.lastLanSeen,
        lastWanSeen: existing.lastWanSeen,
        udpAddress: existing.udpAddress,
        lastDirectReachAt: existing.lastDirectReachAt,
        lanAuthenticated: existing.lanAuthenticated,
        wanAuthenticated: existing.wanAuthenticated,
        // Clear BLE auth only when the last BLE path is gone; a partial drop
        // (one role) leaves the Noise session intact.
        bleAuthenticated: hasAnyBle ? existing.bleAuthenticated : false,
        // Nothing is cached across sessions: the attestation goes out with
        // the session that produced it (spec §Session Establishment).
        bleAttestation: hasAnyBle ? existing.bleAttestation : null,
        lanAttestation: existing.lanAttestation,
        wanAttestation: existing.wanAttestation,
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
      // A fresh session is authenticated but not yet attested; the exchange
      // follows and its outcome arrives as PeerAttestedAction.
      return state.copyWith(
        peers: Map.from(state.peers)
          ..[pubkeyHex] = existing
              .copyWith(bleAuthenticated: true)
              .withoutAttestation(Underlay.pan),
      );
    }
    return state;
  }

  if (action is PeerAttestedAction) {
    final pubkeyHex = _pubkeyToHex(action.publicKey);
    final existing = state.peers[pubkeyHex];
    if (existing == null) return state;
    return state.copyWith(
      peers: Map.from(state.peers)
        ..[pubkeyHex] =
            existing.withAttestation(action.underlay, action.attestation),
    );
  }

  if (action is PeerIpSessionChangedAction) {
    final existing = state.peers[action.pubkeyHex];
    if (existing != null) {
      final updated = existing.withIpSession(
        action.underlay,
        authenticated: action.connected,
      );
      return state.copyWith(
        peers: Map.from(state.peers)..[action.pubkeyHex] = updated,
      );
    }
    return state;
  }

  if (action is PeerIpSeenAction) {
    final pubkeyHex = _pubkeyToHex(action.publicKey);
    final existing = state.peers[pubkeyHex];
    if (existing != null) {
      final now = DateTime.now();
      final updated = action.underlay == Underlay.lan
          ? existing.copyWith(lastSeen: now, lastLanSeen: now)
          : existing.copyWith(lastSeen: now, lastWanSeen: now);
      return state.copyWith(
        peers: Map.from(state.peers)..[pubkeyHex] = updated,
      );
    }
    return state;
  }

  if (action is PeerBleSeenAction) {
    final pubkeyHex = _pubkeyToHex(action.publicKey);
    final existing = state.peers[pubkeyHex];
    // A packet decrypted after both GATT legs dropped must not refresh a peer
    // with no BLE link.
    if (existing != null && existing.hasBleConnection) {
      final now = DateTime.now();
      final updated = existing.copyWith(lastSeen: now, lastBleSeen: now);
      return state.copyWith(
        peers: Map.from(state.peers)..[pubkeyHex] = updated,
      );
    }
    return state;
  }

  if (action is PeerIpDisconnectedAction) {
    final pubkeyHex = _pubkeyToHex(action.publicKey);
    final existing = state.peers[pubkeyHex];
    if (existing != null) {
      // If no other transport, mark as disconnected: no GATT link, and no
      // authenticated session on the other IP underlay.
      final otherIp =
          action.underlay == Underlay.lan ? Underlay.wan : Underlay.lan;
      final newConnectionState =
          existing.hasBleConnection || existing.isAuthenticatedOn(otherIp)
              ? existing.connectionState
              : PeerConnectionState.disconnected;
      // Preserve the UDP address — it's the last known location and needed
      // for reconnection. Never clear peer addresses. The session on this
      // underlay is gone, and its attestation with it; transport
      // independence: BLE and the other IP session are untouched.
      final updated = existing.withIpSession(
        action.underlay,
        authenticated: false,
        connectionState: newConnectionState,
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
    // Memory pressure: forget unprotected peers we haven't heard from within [staleThreshold]. We do NOT mutate `connectionState` here — that field is exclusively driven by plugin events (BLE) and UDP transport events (UDP), so that both sides of a connection see the same transitions. Known peers (GLP-supplied keys, passed as [protectedPubkeyHexes]) are kept regardless so we can reconnect to them later.
    final now = DateTime.now();
    final newMap = Map<String, PeerState>.from(state.peers);
    final staleKeys = <String>[];
    newMap.forEach((key, peer) {
      if (action.protectedPubkeyHexes.contains(key)) return;
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
      // If the address changed, prior reachability observation is stale.
      final preserveReach = newAddress == existing.udpAddress;
      final updated = PeerState(
        publicKey: existing.publicKey,
        connectionState: existing.connectionState,
        transport: existing.transport,
        rssi: existing.rssi,
        protocolVersion: existing.protocolVersion,
        lastSeen: existing.lastSeen,
        bleCentralDeviceId: existing.bleCentralDeviceId,
        blePeripheralDeviceId: existing.blePeripheralDeviceId,
        lastBleSeen: existing.lastBleSeen,
        lastLanSeen: existing.lastLanSeen,
        lastWanSeen: existing.lastWanSeen,
        udpAddress: newAddress,
        lastDirectReachAt: preserveReach ? existing.lastDirectReachAt : null,
        lanAuthenticated: existing.lanAuthenticated,
        wanAuthenticated: existing.wanAuthenticated,
        bleAuthenticated: existing.bleAuthenticated,
        bleAttestation: existing.bleAttestation,
        lanAttestation: existing.lanAttestation,
        wanAttestation: existing.wanAttestation,
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
      connectionState: PeerConnectionState.disconnected,
      transport: PeerTransport.wan,
      udpAddress: newAddress,
    );
    return state.copyWith(
      peers: Map.from(state.peers)..[pubkeyHex] = created,
    );
  }

  if (action is PeerIdentityRegisteredAction) {
    final pubkeyHex = _pubkeyToHex(action.publicKey);
    if (state.peers.containsKey(pubkeyHex)) return state;
    // Identity supplied by GLP (putKnownPeer): a minimal disconnected
    // entry with no address — reachability and addresses arrive later via
    // sessions, ANNOUNCE, or putPeerAddress.
    final created = PeerState(
      publicKey: action.publicKey,
      connectionState: PeerConnectionState.disconnected,
      transport: PeerTransport.wan,
    );
    return state.copyWith(
      peers: Map.from(state.peers)..[pubkeyHex] = created,
    );
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
