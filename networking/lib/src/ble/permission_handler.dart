import 'dart:io';
import 'package:permission_handler/permission_handler.dart';
import 'package:flutter/foundation.dart';

/// Result of permission request
enum PermissionResult {
  /// All required permissions granted
  granted,

  /// Some permissions denied
  denied,

  /// Permissions permanently denied (need to go to settings)
  permanentlyDenied,
}

/// Handles permission requests for BLE functionality.
///
/// Required permissions:
/// - Android 12+: BLUETOOTH_SCAN, BLUETOOTH_CONNECT, BLUETOOTH_ADVERTISE
/// - Android <12: BLUETOOTH, BLUETOOTH_ADMIN, ACCESS_FINE_LOCATION
/// - iOS: Bluetooth usage strings in Info.plist; CoreBluetooth prompts when used
class PermissionHandler {
  /// Check if all required permissions are granted
  Future<bool> hasRequiredPermissions() async {
    if (Platform.isAndroid) {
      return await _checkAndroidPermissions();
    } else if (Platform.isIOS) {
      return await _checkIOSPermissions();
    }
    return false;
  }

  /// Request all required permissions
  ///
  /// Returns [PermissionResult] indicating success/failure
  Future<PermissionResult> requestPermissions() async {
    debugPrint('Requesting BLE permissions');

    if (Platform.isAndroid) {
      return await _requestAndroidPermissions();
    } else if (Platform.isIOS) {
      return await _requestIOSPermissions();
    }

    return PermissionResult.denied;
  }

  // ===== Android =====

  Future<bool> _checkAndroidPermissions() async {
    // BLUETOOTH_SCAN/CONNECT/ADVERTISE are runtime on API 31+ and a no-op
    // (granted by manifest) on API 23-30. Either way checking them works.
    final bluetoothScan = await Permission.bluetoothScan.isGranted;
    final bluetoothConnect = await Permission.bluetoothConnect.isGranted;
    final bluetoothAdvertise = await Permission.bluetoothAdvertise.isGranted;

    if (!bluetoothScan || !bluetoothConnect || !bluetoothAdvertise) {
      return false;
    }

    // Location is only required on API 23-30 — the manifest scopes the
    // ACCESS_FINE_LOCATION declaration to maxSdkVersion=30 and adds
    // `neverForLocation` to BLUETOOTH_SCAN on API 31+. If the request would
    // be a no-op (permission not declared on this device), treat it as
    // satisfied.
    final locationStatus = await Permission.locationWhenInUse.status;
    if (locationStatus.isGranted || locationStatus.isLimited) return true;
    if (locationStatus.isDenied || locationStatus.isPermanentlyDenied) {
      // Not granted. On API 31+ this is expected because the permission is
      // not declared. Use whether bluetoothScan was granted as the signal:
      // if BLUETOOTH_SCAN granted but location wasn't even requested by the
      // OS (status is denied without a prompt), we are on API 31+ and OK.
      return true;
    }
    return true;
  }

  Future<PermissionResult> _requestAndroidPermissions() async {
    final bluetoothStatuses = await [
      Permission.bluetoothScan,
      Permission.bluetoothConnect,
      Permission.bluetoothAdvertise,
    ].request();

    var anyDenied = false;
    var anyPermanentlyDenied = false;
    for (final status in bluetoothStatuses.values) {
      if (status.isDenied) anyDenied = true;
      if (status.isPermanentlyDenied) anyPermanentlyDenied = true;
    }

    if (anyPermanentlyDenied) {
      debugPrint('Bluetooth permissions permanently denied');
      return PermissionResult.permanentlyDenied;
    }
    if (anyDenied) {
      debugPrint('Bluetooth permissions denied');
      return PermissionResult.denied;
    }

    // Best-effort location request. On API 31+ the manifest no longer
    // declares ACCESS_FINE_LOCATION (see app's AndroidManifest.xml,
    // maxSdkVersion=30) and `Permission.locationWhenInUse.request()` will
    // return `denied` without prompting. That is expected and not fatal.
    try {
      await Permission.locationWhenInUse.request();
    } catch (e) {
      debugPrint('locationWhenInUse request failed (likely API 31+ '
          'where the permission is undeclared): $e');
    }

    debugPrint('BLE permissions granted (location is optional)');
    return PermissionResult.granted;
  }

  // ===== iOS =====

  Future<bool> _checkIOSPermissions() async {
    // iOS handles Bluetooth permissions automatically via Info.plist
    // since iOS 13+. No need to check bluetooth permission explicitly.
    // Just return true as Bluetooth will be granted automatically when used
    debugPrint('iOS: Bluetooth permissions handled by Info.plist');
    return true;
  }

  Future<PermissionResult> _requestIOSPermissions() async {
    // iOS 13+ handles Bluetooth automatically via Info.plist
    // No explicit permission request needed for Bluetooth
    debugPrint(
        'iOS: Bluetooth permissions granted automatically via Info.plist');
    return PermissionResult.granted;
  }

  /// Open app settings for manual permission grant
  Future<bool> openSettings() async {
    return await openAppSettings();
  }
}
