/// The platform attestation roots this layer pins.
///
/// Spec `docs/GLP_Networking_API/sections/api.tex` §Session Establishment:
/// "Both attestations are verified by the peer against a public root — Apple's
/// App Attest root, Google's hardware attestation root — so no third party is
/// contacted at the moment two agents establish a session, which is the
/// property the protocol needs."
///
/// Pinning is what makes that true. A verifier that fetched a root would have
/// a fixed global service inside the per-session path, which is exactly what
/// choosing key attestation over Play Integrity was meant to avoid.
library;

import 'dart:convert';
import 'dart:typed_data';

/// Google's hardware attestation roots, BOTH of them.
///
/// Two are pinned and not one. Google's attestation root endpoint publishes
/// two, and the newer is effective 2026-02-01, so from that date a verifier
/// pinning only the older rejects every device provisioned under the newer and
/// one pinning only the newer rejects every device provisioned before it.
/// Either alone fails roughly half the fleet, silently, as an attestation
/// "found invalid" — which tears sessions down.
///
/// Fetched from `https://android.googleapis.com/attestation/root`, Google's own
/// endpoint, on 2026-08-03. Their SHA-256 fingerprints, so a reviewer can check
/// these bytes against Google's without trusting this file:
///
///   RSA-4096, self-signed, serialNumber f92009e853b6b045,
///   2022-03-20 → 2042-03-15, sha256WithRSAEncryption:
///   CE:DB:1C:B6:DC:89:6A:E5:EC:79:73:48:BC:E9:28:67:
///   53:C2:B3:8E:E7:1C:E0:FB:E3:4A:9A:12:48:80:0D:FC
///
///   EC P-384, CN=Key Attestation CA1, O=Google LLC,
///   2025-07-17 → 2035-07-15, ecdsa-with-SHA384:
///   6D:9D:B4:CE:6C:5C:0B:29:31:66:D0:89:86:E0:57:74:
///   A8:77:6C:EB:52:5D:9E:43:29:52:0D:E1:2B:A4:BC:C0
const List<String> androidAttestationRootsPem = [
  // RSA-4096, 2022 → 2042.
  '''
-----BEGIN CERTIFICATE-----
MIIFHDCCAwSgAwIBAgIJAPHBcqaZ6vUdMA0GCSqGSIb3DQEBCwUAMBsxGTAXBgNV
BAUTEGY5MjAwOWU4NTNiNmIwNDUwHhcNMjIwMzIwMTgwNzQ4WhcNNDIwMzE1MTgw
NzQ4WjAbMRkwFwYDVQQFExBmOTIwMDllODUzYjZiMDQ1MIICIjANBgkqhkiG9w0B
AQEFAAOCAg8AMIICCgKCAgEAr7bHgiuxpwHsK7Qui8xUFmOr75gvMsd/dTEDDJdS
Sxtf6An7xyqpRR90PL2abxM1dEqlXnf2tqw1Ne4Xwl5jlRfdnJLmN0pTy/4lj4/7
tv0Sk3iiKkypnEUtR6WfMgH0QZfKHM1+di+y9TFRtv6y//0rb+T+W8a9nsNL/ggj
nar86461qO0rOs2cXjp3kOG1FEJ5MVmFmBGtnrKpa73XpXyTqRxB/M0n1n/W9nGq
C4FSYa04T6N5RIZGBN2z2MT5IKGbFlbC8UrW0DxW7AYImQQcHtGl/m00QLVWutHQ
oVJYnFPlXTcHYvASLu+RhhsbDmxMgJJ0mcDpvsC4PjvB+TxywElgS70vE0XmLD+O
JtvsBslHZvPBKCOdT0MS+tgSOIfga+z1Z1g7+DVagf7quvmag8jfPioyKvxnK/Eg
sTUVi2ghzq8wm27ud/mIM7AY2qEORR8Go3TVB4HzWQgpZrt3i5MIlCaY504LzSRi
igHCzAPlHws+W0rB5N+er5/2pJKnfBSDiCiFAVtCLOZ7gLiMm0jhO2B6tUXHI/+M
RPjy02i59lINMRRev56GKtcd9qO/0kUJWdZTdA2XoS82ixPvZtXQpUpuL12ab+9E
aDK8Z4RHJYYfCT3Q5vNAXaiWQ+8PTWm2QgBR/bkwSWc+NpUFgNPN9PvQi8WEg5Um
AGMCAwEAAaNjMGEwHQYDVR0OBBYEFDZh4QB8iAUJUYtEbEf/GkzJ6k8SMB8GA1Ud
IwQYMBaAFDZh4QB8iAUJUYtEbEf/GkzJ6k8SMA8GA1UdEwEB/wQFMAMBAf8wDgYD
VR0PAQH/BAQDAgIEMA0GCSqGSIb3DQEBCwUAA4ICAQB8cMqTllHc8U+qCrOlg3H7
174lmaCsbo/bJ0C17JEgMLb4kvrqsXZs01U3mB/qABg/1t5Pd5AORHARs1hhqGIC
W/nKMav574f9rZN4PC2ZlufGXb7sIdJpGiO9ctRhiLuYuly10JccUZGEHpHSYM2G
tkgYbZba6lsCPYAAP83cyDV+1aOkTf1RCp/lM0PKvmxYN10RYsK631jrleGdcdkx
oSK//mSQbgcWnmAEZrzHoF1/0gso1HZgIn0YLzVhLSA/iXCX4QT2h3J5z3znluKG
1nv8NQdxei2DIIhASWfu804CA96cQKTTlaae2fweqXjdN1/v2nqOhngNyz1361mF
mr4XmaKH/ItTwOe72NI9ZcwS1lVaCvsIkTDCEXdm9rCNPAY10iTunIHFXRh+7KPz
lHGewCq/8TOohBRn0/NNfh7uRslOSZ/xKbN9tMBtw37Z8d2vvnXq/YWdsm1+JLVw
n6yYD/yacNJBlwpddla8eaVMjsF6nBnIgQOf9zKSe06nSTqvgwUHosgOECZJZ1Eu
zbH4yswbt02tKtKEFhx+v+OTge/06V+jGsqTWLsfrOCNLuA8H++z+pUENmpqnnHo
vaI47gC+TNpkgYGkkBT6B/m/U01BuOBBTzhIlMEZq9qkDWuM2cA5kW5V3FJUcfHn
w1IdYIg2Wxg7yHcQZemFQg==
-----END CERTIFICATE-----
''',
  // EC P-384, CN=Key Attestation CA1, 2025 → 2035.
  '''
-----BEGIN CERTIFICATE-----
MIICIjCCAaigAwIBAgIRAISp0Cl7DrWK5/8OgN52BgUwCgYIKoZIzj0EAwMwUjEc
MBoGA1UEAwwTS2V5IEF0dGVzdGF0aW9uIENBMTEQMA4GA1UECwwHQW5kcm9pZDET
MBEGA1UECgwKR29vZ2xlIExMQzELMAkGA1UEBhMCVVMwHhcNMjUwNzE3MjIzMjE4
WhcNMzUwNzE1MjIzMjE4WjBSMRwwGgYDVQQDDBNLZXkgQXR0ZXN0YXRpb24gQ0Ex
MRAwDgYDVQQLDAdBbmRyb2lkMRMwEQYDVQQKDApHb29nbGUgTExDMQswCQYDVQQG
EwJVUzB2MBAGByqGSM49AgEGBSuBBAAiA2IABCPaI3FO3z5bBQo8cuiEas4HjqCt
G/mLFfRT0MsIssPBEEU5Cfbt6sH5yOAxqEi5QagpU1yX4HwnGb7OtBYpDTB57uH5
Eczm34A5FNijV3s0/f0UPl7zbJcTx6xwqMIRq6NCMEAwDwYDVR0TAQH/BAUwAwEB
/zAOBgNVHQ8BAf8EBAMCAQYwHQYDVR0OBBYEFFIyuyz7RkOb3NaBqQ5lZuA0QepA
MAoGCCqGSM49BAMDA2gAMGUCMETfjPO/HwqReR2CS7p0ZWoD/LHs6hDi422opifH
EUaYLxwGlT9SLdjkVpz0UUOR5wIxAIoGyxGKRHVTpqpGRFiJtQEOOTp/+s1GcxeY
uR2zh/80lQyu9vAFCj6E4AXc+osmRg==
-----END CERTIFICATE-----
''',
];

/// The pinned Android roots as DER.
List<Uint8List> androidAttestationRootsDer() =>
    androidAttestationRootsPem.map(pemToDer).toList(growable: false);

/// Decode a PEM certificate to DER.
///
/// Throws [FormatException] on anything that is not a single PEM certificate.
Uint8List pemToDer(String pem) {
  const begin = '-----BEGIN CERTIFICATE-----';
  const end = '-----END CERTIFICATE-----';
  final start = pem.indexOf(begin);
  final stop = pem.indexOf(end);
  if (start < 0 || stop < 0 || stop < start) {
    throw const FormatException('Not a PEM certificate');
  }
  final body = pem
      .substring(start + begin.length, stop)
      .replaceAll(RegExp(r'\s'), '');
  if (body.isEmpty) throw const FormatException('Empty PEM body');
  return Uint8List.fromList(base64.decode(body));
}
