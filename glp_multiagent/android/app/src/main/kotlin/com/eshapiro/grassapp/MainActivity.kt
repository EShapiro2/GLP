package com.eshapiro.grassapp

import io.flutter.embedding.android.FlutterActivity
import io.flutter.embedding.engine.FlutterEngine
import io.flutter.plugin.common.MethodChannel
import com.example.grassroots_networking.PlaceGeofencePlugin
import com.example.grassroots_networking.PlatformAttestationPlugin
import com.example.grassroots_networking.TransportForegroundService

/**
 * The Grassroots App's activity on Android: the networking layer's platform
 * plugins, compiled from networking/android (app/build.gradle.kts), registered
 * as the layer's own demo app registers them.
 */
class MainActivity : FlutterActivity() {
    /**
     * The place predicates' platform half (spec §System Predicates). Held here
     * because it owns the geofences standing with the platform; releasing it
     * releases them.
     */
    private var placeGeofence: PlaceGeofencePlugin? = null

    /**
     * The attestation exchange's producer (spec §Session Establishment): the
     * keystore's attested key over the identity key, and the per-session
     * signature.
     */
    private var platformAttestation: PlatformAttestationPlugin? = null

    override fun configureFlutterEngine(flutterEngine: FlutterEngine) {
        super.configureFlutterEngine(flutterEngine)
        MethodChannel(
            flutterEngine.dartExecutor.binaryMessenger,
            "grassroots/foreground_service",
        ).setMethodCallHandler { call, result ->
            when (call.method) {
                "start" -> {
                    TransportForegroundService.start(this)
                    result.success(null)
                }
                "stop" -> {
                    TransportForegroundService.stop(this)
                    result.success(null)
                }
                else -> result.notImplemented()
            }
        }

        placeGeofence = PlaceGeofencePlugin.attach(
            this,
            flutterEngine.dartExecutor.binaryMessenger,
        )

        platformAttestation = PlatformAttestationPlugin.attach(
            this,
            flutterEngine.dartExecutor.binaryMessenger,
        )
    }

    // Crossings are observable only while the application holds the
    // foreground — the layer asks for no background location — so the plugin
    // is told, and it tells the layer.
    override fun onResume() {
        super.onResume()
        placeGeofence?.onActivityForeground(true)
    }

    override fun onPause() {
        placeGeofence?.onActivityForeground(false)
        super.onPause()
    }

    override fun onRequestPermissionsResult(
        requestCode: Int,
        permissions: Array<out String>,
        grantResults: IntArray,
    ) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults)
        placeGeofence?.onPermissionsChanged()
    }

    override fun onDestroy() {
        // Nothing is left registered with the platform once the layer is done
        // with it, and the activity going away is one way of being done.
        placeGeofence?.detach()
        placeGeofence = null
        platformAttestation?.detach()
        platformAttestation = null
        super.onDestroy()
    }
}
