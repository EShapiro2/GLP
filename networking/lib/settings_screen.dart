import 'dart:async';

import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:redux/redux.dart';
import 'debug_log_screen.dart';
import 'src/store/app_state.dart';
import 'src/store/settings_actions.dart';
import 'src/store/settings_state.dart';
import 'src/store/transports_state.dart';
import 'src/transport/transport_service.dart';

/// Settings screen for configuring transport protocols
class SettingsScreen extends StatefulWidget {
  final Store<AppState> store;

  /// Callback when settings are changed
  final VoidCallback? onSettingsChanged;
  final Future<bool> Function(String address, String pubkeyHex)?
      onAddRendezvousServer;
  final Future<void> Function(String address, String pubkeyHex)?
      onRemoveRendezvousServer;

  /// Debug-only: switch which BLE roles the local device runs.
  final Future<void> Function(BleRoleMode mode)? onBleRoleModeChanged;

  /// Re-run public-address (seeip) discovery, invoked by the "Retry" button
  /// shown when discovery has failed and no IP is known.
  final Future<void> Function()? onRetryPublicAddressDiscovery;

  const SettingsScreen({
    super.key,
    required this.store,
    this.onSettingsChanged,
    this.onAddRendezvousServer,
    this.onRemoveRendezvousServer,
    this.onBleRoleModeChanged,
    this.onRetryPublicAddressDiscovery,
  });

  @override
  State<SettingsScreen> createState() => _SettingsScreenState();
}

class _SettingsScreenState extends State<SettingsScreen> {
  late bool _bluetoothEnabled;
  late bool _udpEnabled;
  bool _isSavingAnchor = false;
  bool _isRetryingPublicAddressDiscovery = false;
  StreamSubscription<AppState>? _storeSubscription;

  late final TextEditingController _anchorAddressController;
  late final TextEditingController _anchorPubkeyController;

  @override
  void initState() {
    super.initState();
    _bluetoothEnabled = widget.store.state.settings.bluetoothEnabled;
    _udpEnabled = widget.store.state.settings.udpEnabled;

    _anchorAddressController = TextEditingController();
    _anchorPubkeyController = TextEditingController();
    _storeSubscription = widget.store.onChange.listen((state) {
      final settings = state.settings;
      if (_bluetoothEnabled != settings.bluetoothEnabled) {
        _bluetoothEnabled = settings.bluetoothEnabled;
      }
      if (_udpEnabled != settings.udpEnabled) {
        _udpEnabled = settings.udpEnabled;
      }
      if (mounted) {
        setState(() {});
      }
    });
  }

  @override
  void dispose() {
    _storeSubscription?.cancel();
    _anchorAddressController.dispose();
    _anchorPubkeyController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Settings'),
        backgroundColor: const Color(0xFF1B3D2F),
      ),
      body: ListView(
        children: [
          const SizedBox(height: 16),

          // Transport Section Header
          Padding(
            padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
            child: Row(
              children: [
                const Icon(Icons.swap_horiz, color: Color(0xFFE8A33C)),
                const SizedBox(width: 8),
                const Text(
                  'Transport Protocols',
                  style: TextStyle(
                    fontSize: 16,
                    fontWeight: FontWeight.bold,
                    color: Color(0xFFE8A33C),
                  ),
                ),
              ],
            ),
          ),

          const Padding(
            padding: EdgeInsets.symmetric(horizontal: 16),
            child: Text(
              'Choose which protocols to use for peer communication. '
              'Bluetooth is preferred when peers are nearby.',
              style: TextStyle(color: Colors.grey, fontSize: 13),
            ),
          ),

          const SizedBox(height: 8),

          // Bluetooth Toggle
          _buildTransportTile(
            icon: Icons.bluetooth,
            iconColor: Colors.blue,
            title: 'Bluetooth',
            subtitle: 'Connect to nearby peers via BLE',
            value: _bluetoothEnabled,
            available:
                widget.store.state.transports.bleState != TransportState.error,
            onChanged: _onBluetoothChanged,
            priority: 1,
          ),

          // Debug: BLE role mode (auto / central-only / peripheral-only).
          if (_bluetoothEnabled && widget.onBleRoleModeChanged != null)
            _buildBleRoleModeSelector(),

          if (_bluetoothEnabled) _buildColdCallTrustSelector(),

          // UDP Toggle
          _buildTransportTile(
            icon: Icons.public,
            iconColor: Colors.green,
            title: 'Internet',
            subtitle: 'Connect to peers over the Internet',
            value: _udpEnabled,
            available:
                widget.store.state.transports.udpState != TransportState.error,
            onChanged: _onUdpChanged,
            priority: 2,
          ),

          // Internet connection status
          if (_udpEnabled && widget.store.state.transports.udpState.isUsable)
            _buildConnectionStatusBadge(),

          const Divider(height: 32),

          // Anchor Server Section
          _buildAnchorServerSection(),

          const Divider(height: 32),

          // Warning if no transport enabled
          if (!_bluetoothEnabled && !_udpEnabled)
            Container(
              margin: const EdgeInsets.symmetric(horizontal: 16),
              padding: const EdgeInsets.all(16),
              decoration: BoxDecoration(
                color: Colors.red.withOpacity(0.1),
                borderRadius: BorderRadius.circular(12),
                border: Border.all(color: Colors.red.withOpacity(0.3)),
              ),
              child: Row(
                children: [
                  const Icon(Icons.warning, color: Colors.red),
                  const SizedBox(width: 12),
                  Expanded(
                    child: Column(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      children: const [
                        Text(
                          'No transport enabled',
                          style: TextStyle(
                            fontWeight: FontWeight.bold,
                            color: Colors.red,
                          ),
                        ),
                        SizedBox(height: 4),
                        Text(
                          'You won\'t be able to communicate with other peers. '
                          'Enable at least one transport protocol.',
                          style: TextStyle(color: Colors.red, fontSize: 13),
                        ),
                      ],
                    ),
                  ),
                ],
              ),
            ),

          const SizedBox(height: 16),

          // Info Section
          Padding(
            padding: const EdgeInsets.symmetric(horizontal: 16),
            child: _buildInfoCard(),
          ),

          const Divider(height: 32),

          // Debug Logs
          Padding(
            padding: const EdgeInsets.symmetric(horizontal: 16),
            child: ListTile(
              leading: Container(
                padding: const EdgeInsets.all(8),
                decoration: BoxDecoration(
                  color: Colors.purple.withOpacity(0.15),
                  borderRadius: BorderRadius.circular(8),
                ),
                child: const Icon(Icons.bug_report, color: Colors.purple),
              ),
              title: const Text(
                'Debug Logs',
                style: TextStyle(fontWeight: FontWeight.w600),
              ),
              subtitle: Text(
                'View live transport logs',
                style: TextStyle(color: Colors.grey[400], fontSize: 13),
              ),
              trailing: const Icon(Icons.chevron_right, color: Colors.grey),
              onTap: () {
                Navigator.push(
                  context,
                  MaterialPageRoute(
                    builder: (context) => const DebugLogScreen(),
                  ),
                );
              },
            ),
          ),

          const SizedBox(height: 32),
        ],
      ),
    );
  }

  Widget _buildTransportTile({
    required IconData icon,
    required Color iconColor,
    required String title,
    required String subtitle,
    required bool value,
    required bool available,
    required ValueChanged<bool> onChanged,
    required int priority,
  }) {
    final isEnabled = value && available;

    return Card(
      margin: const EdgeInsets.symmetric(horizontal: 16, vertical: 4),
      color: isEnabled ? const Color(0xFF1B3D2F) : null,
      child: ListTile(
        leading: Container(
          padding: const EdgeInsets.all(8),
          decoration: BoxDecoration(
            color: iconColor.withOpacity(isEnabled ? 0.2 : 0.1),
            borderRadius: BorderRadius.circular(8),
          ),
          child: Icon(
            icon,
            color: isEnabled ? iconColor : Colors.grey,
          ),
        ),
        title: Row(
          children: [
            Expanded(
              child: Text(
                title,
                style: TextStyle(
                  fontWeight: FontWeight.w600,
                  color: isEnabled ? Colors.white : Colors.grey,
                ),
              ),
            ),
            if (!available)
              Container(
                padding: const EdgeInsets.symmetric(horizontal: 8, vertical: 2),
                decoration: BoxDecoration(
                  color: Colors.orange.withOpacity(0.2),
                  borderRadius: BorderRadius.circular(4),
                ),
                child: const Text(
                  'Unavailable',
                  style: TextStyle(
                    fontSize: 10,
                    color: Colors.orange,
                  ),
                ),
              ),
            if (available && isEnabled)
              Container(
                padding: const EdgeInsets.symmetric(horizontal: 8, vertical: 2),
                decoration: BoxDecoration(
                  color: Colors.green.withOpacity(0.2),
                  borderRadius: BorderRadius.circular(4),
                ),
                child: Text(
                  'Priority $priority',
                  style: const TextStyle(
                    fontSize: 10,
                    color: Colors.green,
                  ),
                ),
              ),
          ],
        ),
        subtitle: Text(
          !available ? 'Not available on this device' : subtitle,
          style: TextStyle(
            color: isEnabled ? Colors.grey[400] : Colors.grey,
            fontSize: 13,
          ),
        ),
        trailing: Switch(
          value: value,
          onChanged: available ? onChanged : null,
          activeColor: const Color(0xFFE8A33C),
        ),
        onTap: available ? () => onChanged(!value) : null,
      ),
    );
  }

  Widget _buildBleRoleModeSelector() {
    final mode = widget.store.state.settings.bleRoleMode;
    return Padding(
      padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Row(
            children: [
              const Icon(Icons.bug_report_outlined,
                  size: 16, color: Colors.orange),
              const SizedBox(width: 6),
              Text(
                'BLE role (debug)',
                style: TextStyle(
                  fontSize: 13,
                  fontWeight: FontWeight.w600,
                  color: Colors.orange[700],
                ),
              ),
            ],
          ),
          const SizedBox(height: 4),
          Text(
            switch (mode) {
              BleRoleMode.auto =>
                'Scan + advertise. Paths form as each BLE stack allows.',
              BleRoleMode.centralOnly =>
                'Scan only — peers cannot dial us; we only get central paths.',
              BleRoleMode.peripheralOnly =>
                'Advertise only — we never dial; peers reach us as peripheral.',
            },
            style: const TextStyle(fontSize: 12, color: Colors.black54),
          ),
          const SizedBox(height: 8),
          SegmentedButton<BleRoleMode>(
            segments: const [
              ButtonSegment(
                value: BleRoleMode.auto,
                label: Text('Auto'),
                icon: Icon(Icons.swap_horiz),
              ),
              ButtonSegment(
                value: BleRoleMode.centralOnly,
                label: Text('Central'),
                icon: Icon(Icons.arrow_outward),
              ),
              ButtonSegment(
                value: BleRoleMode.peripheralOnly,
                label: Text('Peripheral'),
                icon: Icon(Icons.arrow_downward),
              ),
            ],
            selected: {mode},
            onSelectionChanged: (selection) async {
              if (selection.isEmpty) return;
              final next = selection.first;
              await widget.onBleRoleModeChanged?.call(next);
              widget.onSettingsChanged?.call();
            },
          ),
        ],
      ),
    );
  }

  Widget _buildColdCallTrustSelector() {
    final level = widget.store.state.settings.coldCallTrustLevel;
    return Padding(
      padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Row(
            children: [
              const Icon(Icons.verified_user_outlined,
                  size: 16, color: Color(0xFF1B3D2F)),
              const SizedBox(width: 6),
              Text(
                'Cold calls',
                style: TextStyle(
                  fontSize: 13,
                  fontWeight: FontWeight.w600,
                  color: Colors.green[800],
                ),
              ),
            ],
          ),
          const SizedBox(height: 4),
          const Text(
            'A "cold call" is an unsolicited BLE first-contact attempt from a '
            'nearby peer you have not friended yet. This setting controls '
            'whether you reply.\n\n'
            '• Open — anyone in range can complete the signed ANNOUNCE '
            'handshake, so you can discover and meet new peers. Strangers '
            'learn your public key and nickname, but never your address '
            'or any friend-only metadata.\n'
            '• Closed — first contact from non-friends is refused. Nearby '
            'devices still see your service advertisement, but ANNOUNCE is '
            'not sent and incoming ANNOUNCEs from strangers are dropped, '
            'so unknown peers cannot learn your nickname over BLE.',
            style: TextStyle(fontSize: 12, color: Colors.black54),
          ),
          const SizedBox(height: 8),
          Text(
            switch (level) {
              ColdCallTrustLevel.open =>
                'Currently open: nearby unknown peers can complete first '
                    'contact.',
              ColdCallTrustLevel.closed =>
                'Currently closed: only accepted friends complete BLE first '
                    'contact.',
            },
            style: TextStyle(
              fontSize: 12,
              color: Colors.green[800],
              fontWeight: FontWeight.w600,
            ),
          ),
          const SizedBox(height: 8),
          SegmentedButton<ColdCallTrustLevel>(
            segments: const [
              ButtonSegment(
                value: ColdCallTrustLevel.open,
                label: Text('Open'),
                icon: Icon(Icons.sensors),
              ),
              ButtonSegment(
                value: ColdCallTrustLevel.closed,
                label: Text('Closed'),
                icon: Icon(Icons.lock_outline),
              ),
            ],
            selected: {level},
            onSelectionChanged: (selection) {
              if (selection.isEmpty) return;
              widget.store
                  .dispatch(SetColdCallTrustLevelAction(selection.first));
              widget.onSettingsChanged?.call();
            },
          ),
        ],
      ),
    );
  }

  Widget _buildConnectionStatusBadge() {
    final transports = widget.store.state.transports;
    final isWellConnected = transports.isWellConnected;
    final publicAddress = transports.publicAddress;
    final publicIp = transports.publicIp;
    final connectionType = transports.networkConnectionType;
    final showDiscoveryFailedWarning =
        transports.publicAddressDiscoveryFailed &&
            publicAddress == null &&
            publicIp == null;

    return Card(
      margin: const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
      color: isWellConnected
          ? Colors.green.withOpacity(0.1)
          : Colors.grey.withOpacity(0.1),
      child: Padding(
        padding: const EdgeInsets.all(12),
        child: Row(
          children: [
            Icon(
              isWellConnected ? Icons.language : Icons.shield_outlined,
              color: isWellConnected ? Colors.green : Colors.grey,
              size: 20,
            ),
            const SizedBox(width: 10),
            Expanded(
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Text(
                    isWellConnected ? 'Well-connected' : 'Standard connection',
                    style: TextStyle(
                      fontWeight: FontWeight.w600,
                      fontSize: 13,
                      color: isWellConnected ? Colors.green : Colors.grey[400],
                    ),
                  ),
                  const SizedBox(height: 2),
                  Text(
                    isWellConnected
                        ? 'Your device has a globally routable address and can help friends connect'
                        : 'Your device is behind NAT — connections to friends may require hole-punching',
                    style: TextStyle(
                      fontSize: 11,
                      color: Colors.grey[500],
                    ),
                  ),
                  const SizedBox(height: 6),
                  Row(
                    children: [
                      Icon(
                        _networkTypeIcon(connectionType),
                        size: 14,
                        color: Colors.grey[400],
                      ),
                      const SizedBox(width: 6),
                      Text(
                        'Connection type: ${connectionType.displayName}',
                        style: TextStyle(
                          fontSize: 11,
                          color: Colors.grey[400],
                        ),
                      ),
                    ],
                  ),
                  if (publicIp != null || publicAddress != null) ...[
                    const SizedBox(height: 6),
                    Text(
                      publicAddress ?? publicIp!,
                      style: TextStyle(
                        fontFamily: 'monospace',
                        fontSize: 11,
                        color: Colors.grey[400],
                      ),
                    ),
                  ] else if (showDiscoveryFailedWarning) ...[
                    const SizedBox(height: 6),
                    _buildNoPublicAddressWarning(),
                  ],
                ],
              ),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildNoPublicAddressWarning() {
    return Row(
      children: [
        Icon(
          Icons.warning_amber_rounded,
          size: 14,
          color: Colors.orange[700],
        ),
        const SizedBox(width: 6),
        Expanded(
          child: Text(
            'No public IP address available',
            style: TextStyle(
              fontSize: 11,
              color: Colors.orange[700],
              fontWeight: FontWeight.w500,
            ),
          ),
        ),
        const SizedBox(width: 4),
        SizedBox(
          height: 24,
          child: TextButton.icon(
            onPressed: _isRetryingPublicAddressDiscovery ||
                    widget.onRetryPublicAddressDiscovery == null
                ? null
                : _handleRetryPublicAddressDiscovery,
            icon: _isRetryingPublicAddressDiscovery
                ? const SizedBox(
                    width: 12,
                    height: 12,
                    child: CircularProgressIndicator(strokeWidth: 1.5),
                  )
                : const Icon(Icons.refresh, size: 14),
            label: const Text('Retry', style: TextStyle(fontSize: 11)),
            style: TextButton.styleFrom(
              padding: const EdgeInsets.symmetric(horizontal: 8),
              minimumSize: Size.zero,
              tapTargetSize: MaterialTapTargetSize.shrinkWrap,
              visualDensity: VisualDensity.compact,
            ),
          ),
        ),
      ],
    );
  }

  Future<void> _handleRetryPublicAddressDiscovery() async {
    final callback = widget.onRetryPublicAddressDiscovery;
    if (callback == null) return;
    setState(() {
      _isRetryingPublicAddressDiscovery = true;
    });
    try {
      await callback();
    } finally {
      if (mounted) {
        setState(() {
          _isRetryingPublicAddressDiscovery = false;
        });
      }
    }
  }

  IconData _networkTypeIcon(NetworkConnectionType connectionType) {
    switch (connectionType) {
      case NetworkConnectionType.wifi:
        return Icons.wifi;
      case NetworkConnectionType.cellular:
        return Icons.signal_cellular_alt;
      case NetworkConnectionType.ethernet:
        return Icons.settings_ethernet;
      case NetworkConnectionType.vpn:
        return Icons.lock_outline;
      case NetworkConnectionType.other:
        return Icons.device_hub_outlined;
      case NetworkConnectionType.offline:
        return Icons.portable_wifi_off;
    }
  }

  Widget _buildInfoCard() {
    return Container(
      padding: const EdgeInsets.all(16),
      decoration: BoxDecoration(
        color: Colors.blue.withOpacity(0.1),
        borderRadius: BorderRadius.circular(12),
        border: Border.all(color: Colors.blue.withOpacity(0.2)),
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Row(
            children: const [
              Icon(Icons.info_outline, color: Colors.blue, size: 20),
              SizedBox(width: 8),
              Text(
                'How it works',
                style: TextStyle(
                  fontWeight: FontWeight.bold,
                  color: Colors.blue,
                ),
              ),
            ],
          ),
          const SizedBox(height: 12),
          _buildInfoRow(
            icon: Icons.bluetooth,
            iconColor: Colors.blue,
            text: 'Bluetooth connects you to nearby peers without Internet',
          ),
          const SizedBox(height: 8),
          _buildInfoRow(
            icon: Icons.public,
            iconColor: Colors.green,
            text: 'Internet connects you to peers anywhere in the world',
          ),
          const SizedBox(height: 8),
          _buildInfoRow(
            icon: Icons.priority_high,
            iconColor: const Color(0xFFE8A33C),
            text:
                'When both are available, Bluetooth is preferred for faster communication',
          ),
        ],
      ),
    );
  }

  Widget _buildInfoRow({
    required IconData icon,
    required Color iconColor,
    required String text,
  }) {
    return Row(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Icon(icon, size: 16, color: iconColor),
        const SizedBox(width: 8),
        Expanded(
          child: Text(
            text,
            style: const TextStyle(fontSize: 13),
          ),
        ),
      ],
    );
  }

  Widget _buildAnchorServerSection() {
    final servers = widget.store.state.settings.configuredRendezvousServers;
    final hasAnchor = servers.isNotEmpty;

    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        // Section header
        Padding(
          padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
          child: Row(
            children: [
              const Icon(Icons.cloud_outlined, color: Color(0xFFE8A33C)),
              const SizedBox(width: 8),
              const Text(
                'Rendezvous Server',
                style: TextStyle(
                  fontSize: 16,
                  fontWeight: FontWeight.bold,
                  color: Color(0xFFE8A33C),
                ),
              ),
              if (hasAnchor) ...[
                const SizedBox(width: 8),
                Container(
                  padding:
                      const EdgeInsets.symmetric(horizontal: 8, vertical: 2),
                  decoration: BoxDecoration(
                    color: Colors.green.withOpacity(0.2),
                    borderRadius: BorderRadius.circular(4),
                  ),
                  child: Text(
                    '${servers.length} configured',
                    style: TextStyle(fontSize: 10, color: Colors.green),
                  ),
                ),
              ],
            ],
          ),
        ),

        const Padding(
          padding: EdgeInsets.symmetric(horizontal: 16),
          child: Text(
            'A lightweight server that helps peers find each other for '
            'hole-punching. Add one or more servers below. Server is stored '
            'only after live ANNOUNCE response. IPv6 only.',
            style: TextStyle(color: Colors.grey, fontSize: 13),
          ),
        ),

        if (servers.isNotEmpty) ...[
          const SizedBox(height: 12),
          for (final server in servers) _buildConfiguredAnchorTile(server),
        ],

        const SizedBox(height: 12),

        // Address field
        Padding(
          padding: const EdgeInsets.symmetric(horizontal: 16),
          child: TextField(
            controller: _anchorAddressController,
            style: const TextStyle(fontFamily: 'monospace', fontSize: 14),
            decoration: InputDecoration(
              labelText: 'Server address',
              hintText: '[2600:1234::1]:9514 or 198.51.100.5:9514',
              hintStyle: TextStyle(
                color: Colors.grey[700],
                fontFamily: 'monospace',
                fontSize: 14,
              ),
              prefixIcon: const Icon(Icons.dns_outlined, size: 20),
              border: OutlineInputBorder(
                borderRadius: BorderRadius.circular(8),
              ),
              contentPadding:
                  const EdgeInsets.symmetric(horizontal: 12, vertical: 14),
            ),
          ),
        ),

        const SizedBox(height: 12),

        // Public key field
        Padding(
          padding: const EdgeInsets.symmetric(horizontal: 16),
          child: TextField(
            controller: _anchorPubkeyController,
            style: const TextStyle(fontFamily: 'monospace', fontSize: 14),
            decoration: InputDecoration(
              labelText: 'Server public key',
              hintText: '64-character hex',
              hintStyle: TextStyle(
                color: Colors.grey[700],
                fontFamily: 'monospace',
                fontSize: 14,
              ),
              prefixIcon: const Icon(Icons.key_outlined, size: 20),
              border: OutlineInputBorder(
                borderRadius: BorderRadius.circular(8),
              ),
              contentPadding:
                  const EdgeInsets.symmetric(horizontal: 12, vertical: 14),
            ),
          ),
        ),

        const SizedBox(height: 12),

        // Save / Clear buttons
        Padding(
          padding: const EdgeInsets.symmetric(horizontal: 16),
          child: Row(
            children: [
              Expanded(
                child: ElevatedButton.icon(
                  onPressed: _isSavingAnchor ? null : _onSaveAnchor,
                  icon: _isSavingAnchor
                      ? const SizedBox(
                          width: 18,
                          height: 18,
                          child: CircularProgressIndicator(strokeWidth: 2),
                        )
                      : const Icon(Icons.add_link_outlined, size: 18),
                  label: Text(_isSavingAnchor ? 'Verifying...' : 'Add Server'),
                  style: ElevatedButton.styleFrom(
                    backgroundColor: const Color(0xFF1B3D2F),
                    foregroundColor: const Color(0xFFE8A33C),
                    padding: const EdgeInsets.symmetric(vertical: 12),
                    shape: RoundedRectangleBorder(
                      borderRadius: BorderRadius.circular(8),
                    ),
                  ),
                ),
              ),
            ],
          ),
        ),

        _buildFriendSharedRendezvousTable(),
      ],
    );
  }

  Widget _buildFriendSharedRendezvousTable() {
    final rows = _friendSharedRendezvousRows();

    return Padding(
      padding: const EdgeInsets.fromLTRB(16, 16, 16, 0),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Row(
            children: [
              Icon(
                Icons.group_work_outlined,
                size: 18,
                color: Colors.grey[600],
              ),
              const SizedBox(width: 8),
              const Text(
                'Friend-shared Rendezvous Servers',
                style: TextStyle(fontWeight: FontWeight.w600),
              ),
              if (rows.isNotEmpty) ...[
                const SizedBox(width: 8),
                Container(
                  padding:
                      const EdgeInsets.symmetric(horizontal: 8, vertical: 2),
                  decoration: BoxDecoration(
                    color: const Color(0xFF1B3D2F).withValues(alpha: 0.12),
                    borderRadius: BorderRadius.circular(4),
                  ),
                  child: Text(
                    '${rows.length}',
                    style: const TextStyle(
                      fontSize: 10,
                      color: Color(0xFF1B3D2F),
                    ),
                  ),
                ),
              ],
            ],
          ),
          const SizedBox(height: 8),
          if (rows.isEmpty)
            Container(
              width: double.infinity,
              padding: const EdgeInsets.all(12),
              decoration: BoxDecoration(
                border: Border.all(color: Colors.grey.withValues(alpha: 0.25)),
                borderRadius: BorderRadius.circular(8),
              ),
              child: Text(
                'No friend-shared rendezvous servers',
                style: TextStyle(color: Colors.grey[600], fontSize: 13),
              ),
            )
          else
            Container(
              width: double.infinity,
              decoration: BoxDecoration(
                border: Border.all(color: Colors.grey.withValues(alpha: 0.25)),
                borderRadius: BorderRadius.circular(8),
              ),
              child: ClipRRect(
                borderRadius: BorderRadius.circular(8),
                child: SingleChildScrollView(
                  scrollDirection: Axis.horizontal,
                  child: DataTable(
                    headingRowHeight: 36,
                    dataRowMinHeight: 44,
                    dataRowMaxHeight: 56,
                    columnSpacing: 18,
                    columns: const [
                      DataColumn(label: Text('Friend')),
                      DataColumn(label: Text('Address')),
                      DataColumn(label: Text('Public key')),
                    ],
                    rows: [
                      for (final row in rows)
                        DataRow(
                          cells: [
                            DataCell(_textCell(row.sharedBy)),
                            DataCell(_monoCell(row.address)),
                            DataCell(_monoCell(_shortPubkey(row.pubkeyHex))),
                          ],
                        ),
                    ],
                  ),
                ),
              ),
            ),
        ],
      ),
    );
  }

  List<_FriendSharedRendezvousRow> _friendSharedRendezvousRows() {
    final byServer = <String, _MutableFriendSharedRendezvousRow>{};

    // `knownRvServers` now lives on the friendship record (friendship-scoped
    // and persisted). Iterate accepted friendships and pull each friend's
    // human-readable name from the live PeerState when available so the UI
    // matches the rest of the Friends list (which uses PeerState nicknames).
    for (final friendship in widget.store.state.friendships.friends) {
      final hex = friendship.peerPubkeyHex;
      final peer = widget.store.state.peers.getPeerByPubkeyHex(hex);
      final friendName = peer?.displayName.trim().isNotEmpty == true
          ? peer!.displayName
          : (friendship.nickname?.trim().isNotEmpty == true
              ? friendship.nickname!
              : hex.substring(0, 8));

      for (final entry in friendship.knownRvServers.entries) {
        final pubkeyHex = entry.key.toLowerCase();
        final address = entry.value.trim();
        if (pubkeyHex.isEmpty || address.isEmpty) continue;
        final key = '$pubkeyHex|$address';
        final row = byServer.putIfAbsent(
          key,
          () => _MutableFriendSharedRendezvousRow(
            pubkeyHex: pubkeyHex,
            address: address,
          ),
        );
        row.sharedBy.add(friendName);
      }
    }

    final rows = byServer.values
        .map(
          (row) => _FriendSharedRendezvousRow(
            pubkeyHex: row.pubkeyHex,
            address: row.address,
            sharedBy: (row.sharedBy.toList()..sort()).join(', '),
          ),
        )
        .toList()
      ..sort((a, b) {
        final byAddress = a.address.compareTo(b.address);
        if (byAddress != 0) return byAddress;
        return a.pubkeyHex.compareTo(b.pubkeyHex);
      });

    return rows;
  }

  Widget _textCell(String text) {
    return ConstrainedBox(
      constraints: const BoxConstraints(maxWidth: 220),
      child: Text(
        text,
        overflow: TextOverflow.ellipsis,
        style: const TextStyle(fontSize: 12),
      ),
    );
  }

  Widget _monoCell(String text) {
    return ConstrainedBox(
      constraints: const BoxConstraints(maxWidth: 260),
      child: Text(
        text,
        overflow: TextOverflow.ellipsis,
        style: const TextStyle(fontFamily: 'monospace', fontSize: 12),
      ),
    );
  }

  static String _shortPubkey(String pubkeyHex) {
    return pubkeyHex.length > 16
        ? '${pubkeyHex.substring(0, 16)}...'
        : pubkeyHex;
  }

  Widget _buildConfiguredAnchorTile(RendezvousServerSettings server) {
    final shortPubkey = server.pubkeyHex.length > 16
        ? '${server.pubkeyHex.substring(0, 16)}...'
        : server.pubkeyHex;

    return Card(
      margin: const EdgeInsets.symmetric(horizontal: 16, vertical: 4),
      child: ListTile(
        leading: const Icon(Icons.cloud_done_outlined, color: Colors.green),
        title: Text(
          server.address,
          style: const TextStyle(fontFamily: 'monospace', fontSize: 13),
        ),
        subtitle: Text(
          shortPubkey,
          style: const TextStyle(fontFamily: 'monospace', fontSize: 12),
        ),
        trailing: Row(
          mainAxisSize: MainAxisSize.min,
          children: [
            IconButton(
              tooltip: 'Copy server details',
              icon: const Icon(Icons.copy_outlined),
              onPressed: () => _copyAnchorDetails(server),
            ),
            IconButton(
              tooltip: 'Remove server',
              icon: const Icon(Icons.delete_outline, color: Colors.red),
              onPressed: () => _onRemoveAnchor(server),
            ),
          ],
        ),
      ),
    );
  }

  String _formatAnchorDetails(RendezvousServerSettings server) {
    return '${server.address} ${server.pubkeyHex}';
  }

  Future<void> _copyAnchorDetails(RendezvousServerSettings server) async {
    await Clipboard.setData(
      ClipboardData(text: _formatAnchorDetails(server)),
    );

    if (!mounted) return;

    ScaffoldMessenger.of(context).showSnackBar(
      const SnackBar(content: Text('Rendezvous server details copied')),
    );
  }

  Future<void> _onSaveAnchor() async {
    final address = _anchorAddressController.text.trim();
    final pubkey = _anchorPubkeyController.text.trim();

    if (address.isEmpty) {
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(content: Text('Server address is required')),
      );
      return;
    }

    if (pubkey.isEmpty ||
        pubkey.length != 64 ||
        !RegExp(r'^[0-9a-fA-F]{64}$').hasMatch(pubkey)) {
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(content: Text('Public key must be 64 hex characters')),
      );
      return;
    }

    if (widget.onAddRendezvousServer == null) {
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(
          content: Text('Transport not ready. Cannot verify server yet'),
        ),
      );
      return;
    }

    setState(() {
      _isSavingAnchor = true;
    });

    final saved =
        await widget.onAddRendezvousServer!.call(address, pubkey.toLowerCase());

    if (!mounted) return;

    setState(() {
      _isSavingAnchor = false;
    });

    if (!saved) {
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(
          content: Text('Server did not respond. Not saved'),
        ),
      );
      return;
    }

    _anchorAddressController.clear();
    _anchorPubkeyController.clear();
    widget.onSettingsChanged?.call();

    setState(() {});

    ScaffoldMessenger.of(context).showSnackBar(
      const SnackBar(content: Text('Rendezvous server added')),
    );
  }

  void _onRemoveAnchor(RendezvousServerSettings server) {
    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        title: const Text('Remove Rendezvous Server?'),
        content: Text(
          'Remove ${server.address}? Reconnection after IP changes will use '
          'remaining rendezvous servers, BLE, or friends-based signaling.',
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context),
            child: const Text('Cancel'),
          ),
          TextButton(
            onPressed: () async {
              Navigator.pop(context);
              if (widget.onRemoveRendezvousServer != null) {
                await widget.onRemoveRendezvousServer!(
                  server.address,
                  server.pubkeyHex,
                );
              } else {
                widget.store.dispatch(RemoveRendezvousServerAction(server));
              }
              widget.onSettingsChanged?.call();
              if (mounted) {
                setState(() {});
              }
            },
            child: const Text('Remove', style: TextStyle(color: Colors.red)),
          ),
        ],
      ),
    );
  }

  void _onBluetoothChanged(bool value) {
    // Prevent disabling both transports
    if (!value && !_udpEnabled) {
      _showCannotDisableDialog();
      return;
    }

    setState(() {
      _bluetoothEnabled = value;
    });

    widget.store.dispatch(SetBluetoothEnabledAction(value));
    widget.onSettingsChanged?.call();
  }

  void _onUdpChanged(bool value) {
    // Prevent disabling both transports
    if (!value && !_bluetoothEnabled) {
      _showCannotDisableDialog();
      return;
    }

    setState(() {
      _udpEnabled = value;
    });

    widget.store.dispatch(SetUdpEnabledAction(value));
    widget.onSettingsChanged?.call();
  }

  void _showCannotDisableDialog() {
    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        title: const Text('Cannot Disable'),
        content: const Text(
          'At least one transport protocol must be enabled to communicate with peers.',
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context),
            child: const Text('OK'),
          ),
        ],
      ),
    );
  }
}

class _FriendSharedRendezvousRow {
  final String pubkeyHex;
  final String address;
  final String sharedBy;

  const _FriendSharedRendezvousRow({
    required this.pubkeyHex,
    required this.address,
    required this.sharedBy,
  });
}

class _MutableFriendSharedRendezvousRow {
  final String pubkeyHex;
  final String address;
  final Set<String> sharedBy = {};

  _MutableFriendSharedRendezvousRow({
    required this.pubkeyHex,
    required this.address,
  });
}
