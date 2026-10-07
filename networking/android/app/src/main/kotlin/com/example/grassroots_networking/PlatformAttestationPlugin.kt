package com.example.grassroots_networking

import android.content.Context
import android.content.pm.PackageManager
import android.os.Build
import android.os.Handler
import android.os.Looper
import android.security.keystore.KeyGenParameterSpec
import android.security.keystore.KeyInfo
import android.security.keystore.KeyProperties
import android.security.keystore.StrongBoxUnavailableException
import io.flutter.plugin.common.BinaryMessenger
import io.flutter.plugin.common.MethodCall
import io.flutter.plugin.common.MethodChannel
import java.security.KeyFactory
import java.security.KeyPairGenerator
import java.security.KeyStore
import java.security.PrivateKey
import java.security.Signature
import java.security.spec.ECGenParameterSpec
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

/**
 * The Android producer of the attestation exchange (spec §Session
 * Establishment).
 *
 * "Each agent holds an attestation key generated in its platform's secure
 * element and attested there once---... hardware key attestation on
 * Android---over a challenge naming the agent's identity public key pk.  Per
 * session each side sends that attestation together with a signature by the
 * attestation key over the digest H("glp attest" | pk | h)".
 *
 * On Android the attestation key is an Android Keystore key: EC P-256,
 * generated in StrongBox where the device has it and in the TEE otherwise,
 * with the identity key as its attestation challenge. The keystore holds the
 * key and its certificate chain under an alias named by the identity key, so
 * the attestation is obtained once and read back thereafter. The per-session
 * signature is `SHA256withECDSA` over the digest, DER-encoded. Verification is
 * the core's (`android_key_attestation.dart`); this file produces and nothing
 * else.
 *
 * The channel is `grassroots/attestation`; its two methods are documented on
 * the Dart side, `native_platform_attestation.dart`.
 *
 * Where this device provides no attestation — below API 24, which has no key
 * attestation, or a key the keystore holds outside secure hardware — `attest`
 * answers null, which the peer reports as unattested. A software-held key is
 * not offered: a verifier refuses it as not hardware-backed, so offering it
 * would tear down every session rather than report the peer unattested.
 */
class PlatformAttestationPlugin private constructor(
    private val context: Context,
    messenger: BinaryMessenger,
) : MethodChannel.MethodCallHandler {

    companion object {
        private const val CHANNEL = "grassroots/attestation"
        private const val KEYSTORE = "AndroidKeyStore"
        private const val ALIAS_PREFIX = "grassroots-attestation-"

        fun attach(context: Context, messenger: BinaryMessenger): PlatformAttestationPlugin =
            PlatformAttestationPlugin(context.applicationContext, messenger)
    }

    private val channel = MethodChannel(messenger, CHANNEL)

    /**
     * One worker thread: key generation takes seconds (StrongBox longer) and
     * must not hold the main thread, and serialising it is what makes
     * concurrent sessions at startup generate one key and not several.
     */
    private val worker: ExecutorService = Executors.newSingleThreadExecutor()
    private val main = Handler(Looper.getMainLooper())

    init {
        channel.setMethodCallHandler(this)
    }

    fun detach() {
        channel.setMethodCallHandler(null)
        worker.shutdown()
    }

    override fun onMethodCall(call: MethodCall, result: MethodChannel.Result) {
        val pk = call.argument<ByteArray>("identityPublicKey")
        if (pk == null) {
            result.error("badArguments", "identityPublicKey is required", null)
            return
        }
        when (call.method) {
            "attest" -> onWorker(result) {
                chainFor(pk)?.let { mapOf("platform" to "android", "chain" to it) }
            }
            "sign" -> {
                val digest = call.argument<ByteArray>("digest")
                if (digest == null) {
                    result.error("badArguments", "digest is required", null)
                    return
                }
                onWorker(result) { sign(pk, digest) }
            }
            else -> result.notImplemented()
        }
    }

    private fun onWorker(result: MethodChannel.Result, work: () -> Any?) {
        worker.execute {
            try {
                val value = work()
                main.post { result.success(value) }
            } catch (e: Exception) {
                main.post { result.error("attestationFailed", e.toString(), null) }
            }
        }
    }

    private fun alias(pk: ByteArray): String =
        ALIAS_PREFIX + pk.joinToString("") { "%02x".format(it) }

    private fun keyStore(): KeyStore = KeyStore.getInstance(KEYSTORE).apply { load(null) }

    /** The attestation chain for [pk]'s key, leaf first; generated once. */
    private fun chainFor(pk: ByteArray): List<ByteArray>? {
        // Key attestation, and KeyGenParameterSpec.setAttestationChallenge with
        // it, is API 24.
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.N) return null
        val keyStore = keyStore()
        val alias = alias(pk)
        if (!keyStore.containsAlias(alias)) {
            generate(alias, pk)
            if (!inSecureHardware(keyStore, alias)) {
                keyStore.deleteEntry(alias)
                return null
            }
        }
        val chain = keyStore.getCertificateChain(alias) ?: return null
        return chain.map { it.encoded }
    }

    private fun generate(alias: String, pk: ByteArray) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P &&
            context.packageManager.hasSystemFeature(PackageManager.FEATURE_STRONGBOX_KEYSTORE)
        ) {
            try {
                generate(alias, pk, strongBox = true)
                return
            } catch (e: StrongBoxUnavailableException) {
                // The feature is declared and the secure element refused:
                // the TEE below.
            }
        }
        generate(alias, pk, strongBox = false)
    }

    private fun generate(alias: String, pk: ByteArray, strongBox: Boolean) {
        val builder = KeyGenParameterSpec.Builder(alias, KeyProperties.PURPOSE_SIGN)
            .setAlgorithmParameterSpec(ECGenParameterSpec("secp256r1"))
            .setDigests(KeyProperties.DIGEST_SHA256)
            // The challenge names the agent's identity key: the attestation
            // is "over a challenge naming the agent's identity public key pk".
            .setAttestationChallenge(pk)
        if (strongBox && Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
            builder.setIsStrongBoxBacked(true)
        }
        KeyPairGenerator.getInstance(KeyProperties.KEY_ALGORITHM_EC, KEYSTORE).apply {
            initialize(builder.build())
            generateKeyPair()
        }
    }

    /** Whether the keystore holds [alias]'s key in secure hardware. */
    private fun inSecureHardware(keyStore: KeyStore, alias: String): Boolean {
        val key = keyStore.getKey(alias, null) as? PrivateKey ?: return false
        val info = KeyFactory.getInstance(key.algorithm, KEYSTORE)
            .getKeySpec(key, KeyInfo::class.java)
        return if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            when (info.securityLevel) {
                KeyProperties.SECURITY_LEVEL_TRUSTED_ENVIRONMENT,
                KeyProperties.SECURITY_LEVEL_STRONGBOX,
                KeyProperties.SECURITY_LEVEL_UNKNOWN_SECURE -> true
                else -> false
            }
        } else {
            @Suppress("DEPRECATION")
            info.isInsideSecureHardware
        }
    }

    /** `SHA256withECDSA` over [digest] by [pk]'s attested key, DER-encoded. */
    private fun sign(pk: ByteArray, digest: ByteArray): ByteArray? {
        val key = keyStore().getKey(alias(pk), null) as? PrivateKey ?: return null
        return Signature.getInstance("SHA256withECDSA").run {
            initSign(key)
            update(digest)
            sign()
        }
    }
}
