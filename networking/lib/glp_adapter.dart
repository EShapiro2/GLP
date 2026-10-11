/// The runtime's network interface over the networking layer (GNA's backend
/// glue): [GrassrootsGlpNetwork] implements glp_runtime's `GlpNetwork` over
/// [GrassrootsNetwork].  A library of its own, so that the layer's library
/// does not carry the runtime's names, several of which (`Underlay`,
/// `ProximityUnderlay`, `PlaceEvent`) the two share.
library;

export 'src/glp/grassroots_glp_network.dart';
