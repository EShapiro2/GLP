# GAS on a phone

One person, the phone's owner, runs one grassroots contract's canonical platform as an agent of madGLP, over GNA's networking layer: Bluetooth (PAN) and the local network (LAN), the person named by the layer's key. The screen is the GAS app page for one person (`../web/app.html?solo`), served by the app's own server on the phone's loopback. The people the layer finds near the phone are the persons the page offers. The button at the top right (connected/found) opens what the layer sees.

People who are not near each other meet through talmonlab's relay (`https://talmonlab.bgu.ac.il/gasrelay`, source in the talmonlab repo, `server/gasrelay`): one shares their invite link or QR code (the person+ button), the other adds it, and messages between them go through the relay, encrypted end to end between their keys. The relay sees who writes to whom, when and how much, and holds a message until the recipient's agent acknowledges it. A switch in the same sheet turns the relay off, leaving the phone to the people near it.

This is the first test bed of madGLP on phones: no rendezvous server (WAN: inbound UDP does not reach talmonlab through BGU's network), no attestation (every peer is reported unattested), and first contact Open on both proximity underlays.

## What is where

- `lib/person_host.dart`: the person's agent, `AgentRuntime` over a given `GlpNetwork`, running the live harness's `live_person/3` (told `me(Key)` first), and the state the page reads.
- `lib/local_server.dart`: the page and its API, `/api/<contract>/{state,act,day}`, on 127.0.0.1.
- `lib/contracts.dart`: the contracts as packages, each named by its SHA-256: the starters the app carries (`tool/packages.py` writes them into the bundle), the others from GAS's catalog (talmonlab's downloads), and apps made with GAS's app maker from GAS's made apps; each checked against its hash before it is installed, beside the other worlds; and the links GAS shares, `get.html#<hash>`, `add.html#<key>&c=<hash>`, `gas://get/...`, `gas://add/...`.
- `lib/relay_network.dart`: the network the agent runs over, `RelayGlpNetwork`: GNA's layer for the people near the phone, the relay for the others, behind the same `GlpNetwork`; nothing of the layer changes.
- `lib/main.dart`: unpacks the bundle, lets the person pick a contract, starts the layer (`GrassrootsNetwork` through `GrassrootsGlpNetwork`), the agent and the server, and shows the page in a web view.
- `tool/sync_assets.sh`: bundles the GLP files the harnesses load and the GAS site into `assets/` (gitignored).
- `test/pair_test.dart`: two persons of GKK, each over its own headless layer on the loopback, acquaint each other. `GAS_PROGRAMS=$PWD/assets/tree/programs flutter test test/pair_test.dart` runs it from the bundle alone.
- `test/relay_test.dart`: two persons whose layers never meet, acquainted through the relay: on the loopback (`GASRELAY=<talmonlab>/server/gasrelay/bin/relay.dart`) or the running one (`GASRELAY_URL=https://talmonlab.bgu.ac.il/gasrelay`).
- `test/demo_test.dart`: the same pair kept running with the app's own servers, to play in a browser (`GAS_DEMO=gkk flutter test test/demo_test.dart`, then http://127.0.0.1:8201/app.html?c=gkk&solo and :8202).

## Build

On a Mac with Xcode, Flutter (https://docs.flutter.dev/get-started/install/macos), CocoaPods and Python 3; for Android, Android Studio with an SDK.

    git clone -b GAS-twophone https://github.com/nimrodtalmon/GLP.git
    cd GLP/servers/gas
    python3 -I tools/build.py        # the worlds and the site
    cd phone
    bash tool/sync_assets.sh         # the bundle
    flutter pub get

Then one of:

    flutter run -d macos             # the Mac as a device
    flutter run --release -d <iPhone or Android device id>   # see: flutter devices

An iPhone needs, once: Developer Mode on the phone, and in Xcode (`open ios/Runner.xcworkspace`) the target Runner's Signing & Capabilities set to your team (a free Apple ID works for your own phones) with a bundle identifier of your own.

## A first test

Two devices on the same Wi-Fi, or near each other with Bluetooth on. On both, pick the same contract (GKK first), allow Bluetooth and the local network when asked, and wait until the top right shows a connected person. Then on each tap Establish; on one tap Acquaint, pick the other person, Propose; on the other, Accept. Each now lists the other under Contacts.

## Far apart

Each phone, anywhere with Internet: pick the same contract, tap the person+ button at the top; one taps Share (or shows the QR code), the other pastes the link (or scans the code) and taps Add. The other phone says it was added, and the person shows with a 🌐 among the people. Then Establish, Acquaint and Accept as above.
