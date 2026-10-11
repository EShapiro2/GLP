/// The runtime's network interface over the headless layer (GNA's backend
/// glue): [HeadlessGlpNetwork] implements glp_runtime's `GlpNetwork` over
/// [HeadlessGrassrootsNetwork].  A library of its own, so that the layer's
/// library does not carry the runtime's names, several of which (`Underlay`,
/// `ProximityUnderlay`, `PlaceEvent`) the two share.
library;

export 'src/glp/headless_glp_network.dart';
