package il.ac.bgu.talmonlab.gas_phone

import io.flutter.embedding.android.FlutterActivity
import io.flutter.embedding.engine.FlutterEngine

class MainActivity : FlutterActivity() {
    /**
     * The attestation exchange's producer (GNA §2.6), as GNA's own app holds
     * it (PlatformAttestationPlugin.kt, copied from networking/android): the
     * keystore's attested key over the identity key, and the per-session
     * signature, on the channel grassroots/attestation.
     */
    private var platformAttestation: PlatformAttestationPlugin? = null

    override fun configureFlutterEngine(flutterEngine: FlutterEngine) {
        super.configureFlutterEngine(flutterEngine)
        platformAttestation = PlatformAttestationPlugin.attach(
            this,
            flutterEngine.dartExecutor.binaryMessenger,
        )
    }

    override fun onDestroy() {
        platformAttestation?.detach()
        platformAttestation = null
        super.onDestroy()
    }
}
