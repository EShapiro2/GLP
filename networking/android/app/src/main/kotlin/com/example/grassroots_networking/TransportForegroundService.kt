package com.example.grassroots_networking

import android.app.Notification
import android.app.NotificationChannel
import android.app.NotificationManager
import android.app.PendingIntent
import android.app.Service
import android.content.Context
import android.content.Intent
import android.content.pm.ServiceInfo
import android.os.Build
import android.os.IBinder

/**
 * Keeps the app process alive while the Grassroots transport stack runs.
 *
 * Without a foreground service, modern Android freezes the cached process
 * within minutes of backgrounding / screen-off. A frozen Dart VM sends no
 * ANNOUNCEs (peers see us go stale), processes no scan results (the reverse
 * BLE leg never dials), and stalls UDP keepalives — while the radio links
 * themselves stay up, desynchronizing peers' view from the link state.
 *
 * The service does no work itself; its foreground status (with the
 * `connectedDevice` type, matching our persistent BLE GATT connections) is
 * what exempts the process from cached-app freezing.
 *
 * Started/stopped from Dart via the `grassroots/foreground_service` channel
 * (see lib/src/platform/transport_foreground_service.dart) when the
 * transport stack starts/stops.
 */
class TransportForegroundService : Service() {
    override fun onBind(intent: Intent?): IBinder? = null

    override fun onCreate() {
        super.onCreate()
        createChannel()
    }

    override fun onStartCommand(intent: Intent?, flags: Int, startId: Int): Int {
        val notification = buildNotification()
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            startForeground(
                NOTIFICATION_ID,
                notification,
                ServiceInfo.FOREGROUND_SERVICE_TYPE_CONNECTED_DEVICE,
            )
        } else {
            startForeground(NOTIFICATION_ID, notification)
        }
        // NOT_STICKY: the service protects a running Flutter engine; if the
        // OS kills the process anyway, restarting the bare service would only
        // show a zombie notification with no transports behind it.
        return START_NOT_STICKY
    }

    private fun createChannel() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            val channel = NotificationChannel(
                CHANNEL_ID,
                "Peer connectivity",
                NotificationManager.IMPORTANCE_LOW,
            ).apply {
                description = "Keeps Grassroots connected to nearby peers"
                setShowBadge(false)
            }
            val manager =
                getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
            manager.createNotificationChannel(channel)
        }
    }

    private fun buildNotification(): Notification {
        val launchIntent = packageManager.getLaunchIntentForPackage(packageName)
        val contentIntent = launchIntent?.let {
            PendingIntent.getActivity(
                this,
                0,
                it,
                PendingIntent.FLAG_IMMUTABLE or PendingIntent.FLAG_UPDATE_CURRENT,
            )
        }
        val builder = if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            Notification.Builder(this, CHANNEL_ID)
        } else {
            @Suppress("DEPRECATION")
            Notification.Builder(this)
        }
        return builder
            .setContentTitle("Grassroots is online")
            .setContentText("Maintaining peer-to-peer connections")
            .setSmallIcon(R.mipmap.ic_launcher)
            .setContentIntent(contentIntent)
            .setOngoing(true)
            .build()
    }

    companion object {
        private const val CHANNEL_ID = "grassroots_transport"
        private const val NOTIFICATION_ID = 0x6752

        fun start(context: Context) {
            val intent = Intent(context, TransportForegroundService::class.java)
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                context.startForegroundService(intent)
            } else {
                context.startService(intent)
            }
        }

        fun stop(context: Context) {
            context.stopService(Intent(context, TransportForegroundService::class.java))
        }
    }
}
