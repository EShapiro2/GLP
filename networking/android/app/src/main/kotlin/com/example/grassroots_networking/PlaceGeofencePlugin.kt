package com.example.grassroots_networking

import android.Manifest
import android.app.Activity
import android.app.PendingIntent
import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.content.IntentFilter
import android.content.pm.PackageManager
import android.location.Location
import android.os.Build
import android.util.Log
import androidx.core.app.ActivityCompat
import androidx.core.content.ContextCompat
import com.google.android.gms.location.Geofence
import com.google.android.gms.location.GeofencingClient
import com.google.android.gms.location.GeofencingEvent
import com.google.android.gms.location.GeofencingRequest
import com.google.android.gms.location.LocationServices
import com.google.android.gms.location.Priority
import io.flutter.plugin.common.BinaryMessenger
import io.flutter.plugin.common.EventChannel
import io.flutter.plugin.common.MethodCall
import io.flutter.plugin.common.MethodChannel

/**
 * The Android half of the place predicates (spec §System Predicates).
 *
 * GLP supplies a name and a radius; the layer registers a circular geofence
 * around the device's current location and reports the crossings the platform
 * observes. No coordinate ever crosses back: the channel carries a
 * registration identifier and `entered`/`exited`, and nothing else.
 *
 * Location is foreground-only, by a standing decision of this project: the
 * manifest declares no ACCESS_BACKGROUND_LOCATION, so the admissible
 * transitions are ENTER and EXIT delivered while the application holds the
 * foreground. DWELL is not used — it asks the platform to keep watching for a
 * loitering period, which is precisely what foreground-only cannot promise.
 * Reporting stopping when the application leaves the foreground, or when the
 * permission is withdrawn, is what the observability stream is for.
 */
class PlaceGeofencePlugin(
    private val activity: Activity,
    messenger: BinaryMessenger,
) {
    private val context: Context = activity.applicationContext

    private val geofencing: GeofencingClient =
        LocationServices.getGeofencingClient(context)
    private val locationClient =
        LocationServices.getFusedLocationProviderClient(context)

    private val methodChannel =
        MethodChannel(messenger, "grassroots/places")
    private val crossingChannel =
        EventChannel(messenger, "grassroots/places/crossings")
    private val observabilityChannel =
        EventChannel(messenger, "grassroots/places/observability")

    private var crossingSink: EventChannel.EventSink? = null
    private var observabilitySink: EventChannel.EventSink? = null

    /** The registrations standing, so disposal can release every one. */
    private val standing = mutableSetOf<String>()

    /** Whether the application currently holds the foreground. */
    private var isActive = true

    /** Registrations waiting on the runtime permission prompt. */
    private val pending =
        mutableListOf<Triple<String, Double, MethodChannel.Result>>()
    private var awaitingPermission = false

    private val crossingReceiver = object : BroadcastReceiver() {
        override fun onReceive(receiverContext: Context?, intent: Intent?) {
            handleCrossingIntent(intent ?: return)
        }
    }

    private val geofencePendingIntent: PendingIntent by lazy {
        val intent = Intent(context, GeofenceBroadcastReceiver::class.java)
        PendingIntent.getBroadcast(
            context,
            0,
            intent,
            PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_MUTABLE,
        )
    }

    init {
        methodChannel.setMethodCallHandler(::handle)

        crossingChannel.setStreamHandler(object : EventChannel.StreamHandler {
            override fun onListen(arguments: Any?, events: EventChannel.EventSink?) {
                crossingSink = events
            }

            override fun onCancel(arguments: Any?) {
                crossingSink = null
            }
        })

        observabilityChannel.setStreamHandler(object : EventChannel.StreamHandler {
            override fun onListen(arguments: Any?, events: EventChannel.EventSink?) {
                observabilitySink = events
                // The current state at once: a fresh stream must not be read
                // as "nothing has happened yet" when the truth is that
                // nothing is being watched.
                events?.success(isObservable())
            }

            override fun onCancel(arguments: Any?) {
                observabilitySink = null
            }
        })

        // Geofences registered under this PendingIntent outlive the process:
        // Play Services holds them, and a process killed without onDestroy
        // leaves them standing. A fresh start holds no declarations, so
        // anything still registered is residue and goes.
        geofencing.removeGeofences(geofencePendingIntent)

        val filter = IntentFilter(GeofenceBroadcastReceiver.ACTION_CROSSING)
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
            context.registerReceiver(
                crossingReceiver, filter, Context.RECEIVER_NOT_EXPORTED
            )
        } else {
            @Suppress("UnspecifiedRegisterReceiverFlag")
            context.registerReceiver(crossingReceiver, filter)
        }
    }

    // ===== Method channel =====

    private fun handle(call: MethodCall, result: MethodChannel.Result) {
        when (call.method) {
            "register" -> {
                val registrationId = call.argument<String>("registrationId")
                val radius = call.argument<Double>("radiusMetres")
                if (registrationId == null || radius == null) {
                    result.error(
                        "bad-arguments",
                        "register needs registrationId and radiusMetres",
                        null,
                    )
                    return
                }
                register(registrationId, radius, result)
            }

            "unregister" -> {
                val registrationId = call.argument<String>("registrationId")
                if (registrationId == null) {
                    result.error(
                        "bad-arguments", "unregister needs registrationId", null
                    )
                    return
                }
                unregister(registrationId)
                result.success(null)
            }

            "dispose" -> {
                releaseEverything()
                result.success(null)
            }

            else -> result.notImplemented()
        }
    }

    private fun register(
        registrationId: String,
        radiusMetres: Double,
        result: MethodChannel.Result,
    ) {
        if (!radiusMetres.isFinite() || radiusMetres <= 0) {
            result.success(false)
            return
        }
        if (!hasLocationPermission()) {
            if (permissionRefused) {
                Log.i(TAG, "Location permission withheld; refusing $registrationId")
                result.success(false)
                return
            }
            // Hold the registration until the user has answered, as iOS does:
            // refusing a first declaration merely because nobody had asked yet
            // would make the seam unusable.
            pending.add(Triple(registrationId, radiusMetres, result))
            requestPermissionIfNeeded()
            return
        }
        if (standing.size >= GEOFENCE_LIMIT) {
            Log.i(TAG, "Android registers at most $GEOFENCE_LIMIT geofences; " +
                "refusing $registrationId")
            result.success(false)
            return
        }

        // A geofence is centred on the device's current location, so a fix is
        // needed before one can be registered; without one the platform
        // refuses, which is what the layer above is told.
        currentLocation { fix ->
            if (fix == null) {
                Log.i(TAG, "No location fix; refusing $registrationId")
                result.success(false)
                return@currentLocation
            }
            addGeofence(registrationId, radiusMetres, fix, result)
        }
    }

    private fun addGeofence(
        registrationId: String,
        radiusMetres: Double,
        fix: Location,
        result: MethodChannel.Result,
    ) {
        val geofence = Geofence.Builder()
            .setRequestId(registrationId)
            .setCircularRegion(fix.latitude, fix.longitude, radiusMetres.toFloat())
            .setExpirationDuration(Geofence.NEVER_EXPIRE)
            .setTransitionTypes(
                Geofence.GEOFENCE_TRANSITION_ENTER or
                    Geofence.GEOFENCE_TRANSITION_EXIT
            )
            .build()

        val request = GeofencingRequest.Builder()
            // No initial trigger: the geofence is centred where the device is,
            // so it would fire ENTER at once, and being already inside is not
            // a crossing.
            .setInitialTrigger(0)
            .addGeofence(geofence)
            .build()

        try {
            geofencing.addGeofences(request, geofencePendingIntent)
                .addOnSuccessListener {
                    standing.add(registrationId)
                    result.success(true)
                }
                .addOnFailureListener { error ->
                    Log.w(TAG, "Platform refused $registrationId: ${error.message}")
                    result.success(false)
                }
        } catch (e: SecurityException) {
            Log.w(TAG, "Location permission withdrawn mid-registration: ${e.message}")
            result.success(false)
        }
    }

    private fun unregister(registrationId: String) {
        if (!standing.remove(registrationId)) return
        geofencing.removeGeofences(listOf(registrationId))
    }

    /**
     * Release every registration. Platforms bound how many geofences an
     * application may register, so nothing is left registered once the layer
     * is done with it.
     */
    fun releaseEverything() {
        if (standing.isNotEmpty()) {
            geofencing.removeGeofences(standing.toList())
            standing.clear()
        }
        // Anything registered under this process's PendingIntent that we have
        // lost track of goes too.
        geofencing.removeGeofences(geofencePendingIntent)
    }

    /** Release the plugin itself, with everything standing. */
    fun detach() {
        releaseEverything()
        for ((_, _, result) in pending) result.success(false)
        pending.clear()
        methodChannel.setMethodCallHandler(null)
        crossingChannel.setStreamHandler(null)
        observabilityChannel.setStreamHandler(null)
        try {
            context.unregisterReceiver(crossingReceiver)
        } catch (e: IllegalArgumentException) {
            // Never registered, or already gone.
        }
    }

    // ===== Location =====

    private fun currentLocation(then: (Location?) -> Unit) {
        try {
            locationClient
                .getCurrentLocation(Priority.PRIORITY_BALANCED_POWER_ACCURACY, null)
                .addOnSuccessListener { fix ->
                    if (fix != null) {
                        then(fix)
                    } else {
                        // A current-location request can come back empty; the
                        // last known fix is the only other honest answer.
                        locationClient.lastLocation
                            .addOnSuccessListener { last -> then(last) }
                            .addOnFailureListener { then(null) }
                    }
                }
                .addOnFailureListener { error ->
                    Log.w(TAG, "Location request failed: ${error.message}")
                    then(null)
                }
        } catch (e: SecurityException) {
            Log.w(TAG, "Location permission withheld: ${e.message}")
            then(null)
        }
    }

    /**
     * Whether the user has answered the permission prompt with a refusal. Set
     * when a request comes back without a grant, so the next declaration is
     * refused outright rather than prompting again.
     */
    private var permissionRefused = false

    private fun requestPermissionIfNeeded() {
        if (awaitingPermission) return
        awaitingPermission = true
        ActivityCompat.requestPermissions(
            activity,
            arrayOf(
                Manifest.permission.ACCESS_FINE_LOCATION,
                Manifest.permission.ACCESS_COARSE_LOCATION,
            ),
            LOCATION_PERMISSION_REQUEST,
        )
    }

    private fun hasLocationPermission(): Boolean =
        ContextCompat.checkSelfPermission(
            context, Manifest.permission.ACCESS_FINE_LOCATION
        ) == PackageManager.PERMISSION_GRANTED ||
            ContextCompat.checkSelfPermission(
                context, Manifest.permission.ACCESS_COARSE_LOCATION
            ) == PackageManager.PERMISSION_GRANTED

    // ===== Observability =====

    /**
     * Whether the platform is reporting crossings at all: it needs both the
     * permission and the foreground, since the layer asks for no background
     * location.
     */
    private fun isObservable(): Boolean = hasLocationPermission() && isActive

    private fun emitObservability() {
        observabilitySink?.success(isObservable())
    }

    /** The hosting activity resumed or paused. */
    fun onActivityForeground(active: Boolean) {
        if (isActive == active) return
        isActive = active
        emitObservability()
    }

    /**
     * A runtime permission result arrived. Registrations held for it are
     * answered — carried through where the permission was granted, refused
     * where it was not.
     */
    fun onPermissionsChanged() {
        awaitingPermission = false
        emitObservability()

        val waiting = pending.toList()
        pending.clear()
        if (hasLocationPermission()) {
            for ((registrationId, radius, result) in waiting) {
                register(registrationId, radius, result)
            }
        } else {
            permissionRefused = true
            for ((_, _, result) in waiting) result.success(false)
        }
    }

    // ===== Crossings =====

    private fun handleCrossingIntent(intent: Intent) {
        if (!isObservable()) return
        val ids = intent.getStringArrayListExtra(
            GeofenceBroadcastReceiver.EXTRA_REGISTRATION_IDS
        ) ?: return
        val crossing = intent.getStringExtra(
            GeofenceBroadcastReceiver.EXTRA_CROSSING
        ) ?: return

        for (registrationId in ids) {
            // A crossing of a registration this plugin no longer holds is
            // dropped here as well as above: the platform's report can race
            // the removal.
            if (!standing.contains(registrationId)) continue
            crossingSink?.success(
                mapOf(
                    "registrationId" to registrationId,
                    "crossing" to crossing,
                )
            )
        }
    }

    companion object {
        private const val TAG = "place"

        /** Android registers at most 100 geofences per application. */
        private const val GEOFENCE_LIMIT = 100

        private const val LOCATION_PERMISSION_REQUEST = 0x91ace

        fun attach(activity: Activity, messenger: BinaryMessenger) =
            PlaceGeofencePlugin(activity, messenger)
    }
}

/**
 * Receives the platform's geofence transitions and forwards them to the
 * plugin as a local broadcast.
 *
 * A separate manifest-declared receiver because that is what a geofence
 * PendingIntent targets; it holds no state of its own, and translates a
 * platform transition into the two the layer knows.
 */
class GeofenceBroadcastReceiver : BroadcastReceiver() {
    override fun onReceive(context: Context, intent: Intent) {
        val event = GeofencingEvent.fromIntent(intent) ?: return
        if (event.hasError()) {
            Log.w(TAG, "Geofencing event error code ${event.errorCode}")
            return
        }

        val crossing = when (event.geofenceTransition) {
            Geofence.GEOFENCE_TRANSITION_ENTER -> "entered"
            Geofence.GEOFENCE_TRANSITION_EXIT -> "exited"
            else -> return
        }

        val ids = ArrayList(
            event.triggeringGeofences.orEmpty().map { it.requestId }
        )
        if (ids.isEmpty()) return

        context.sendBroadcast(
            Intent(ACTION_CROSSING)
                .setPackage(context.packageName)
                .putStringArrayListExtra(EXTRA_REGISTRATION_IDS, ids)
                .putExtra(EXTRA_CROSSING, crossing)
        )
    }

    companion object {
        private const val TAG = "place"

        const val ACTION_CROSSING =
            "com.example.grassroots_networking.PLACE_CROSSING"
        const val EXTRA_REGISTRATION_IDS = "registrationIds"
        const val EXTRA_CROSSING = "crossing"
    }
}
