# Build stage
FROM dart:stable AS build

WORKDIR /app

# Build context is bootstrap_anchor/. grassroots_dart_udx is pulled from
# pub.dev during `dart pub get`, so no sibling directory needs to be copied
# in.

# Copy pubspec and resolve dependencies first (better layer caching).
COPY pubspec.yaml .
RUN dart pub get

# Copy source
COPY lib/ lib/
COPY bin/ bin/

# Compile to native executable
RUN dart compile exe bin/server.dart -o bin/rendezvous_server

# Runtime stage — minimal image
FROM debian:bookworm-slim

# libsodium23 provides the native library loaded via FFI for Noise static-key
# derivation (Ed25519↔X25519 birational map). Without it the server aborts at
# startup when initializing libsodium.
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    libsodium23 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=build /app/bin/rendezvous_server /app/rendezvous_server

# Identity file persisted via volume mount
VOLUME /app/data

EXPOSE 9514/udp
EXPOSE 9516/udp

ENTRYPOINT ["/app/rendezvous_server"]
CMD ["--identity", "/app/data/identity.json", "--ipv6-port", "9516"]
