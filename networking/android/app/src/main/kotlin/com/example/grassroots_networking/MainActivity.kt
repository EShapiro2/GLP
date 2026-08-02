package com.example.grassroots_networking

import io.flutter.embedding.android.FlutterActivity
import io.flutter.embedding.engine.FlutterEngine
import io.flutter.plugin.common.MethodChannel

class MainActivity : FlutterActivity() {
    /**
     * The place predicates' platform half (spec §System Predicates). Held here
     * because it owns the geofences standing with the platform; releasing it
     * releases them.
     */
    private var placeGeofence: PlaceGeofencePlugin? = null

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
        super.onDestroy()
    }
}
