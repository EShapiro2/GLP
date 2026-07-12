package com.example.grassroots_networking

import io.flutter.embedding.android.FlutterActivity
import io.flutter.embedding.engine.FlutterEngine
import io.flutter.plugin.common.MethodChannel

class MainActivity : FlutterActivity() {
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
    }
}
