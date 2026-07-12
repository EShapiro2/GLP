#!/bin/bash
#
# Deploy the GLP Rendezvous Server to GCP.
#
# Prerequisites:
#   - gcloud CLI installed and authenticated
#   - Docker installed
#   - A GCP project with Artifact Registry and Compute Engine enabled
#
# Usage:
#   ./deploy.sh [PROJECT_ID] [REGION] [ZONE]
#
# The build context is the bootstrap_anchor directory itself.
# grassroots_dart_udx (the UDX dependency, formerly vendored from
# ../dart-udx) is now pulled from pub.dev during `dart pub get` inside
# the build stage — no sibling source tree is required.
#
# Local build / smoke-test (does not deploy):
#   cd bootstrap_anchor
#   docker build -t rendezvous-server .
#   docker run -p 9514:9514/udp -p 9516:9516/udp \
#     -v "$(pwd)/data:/app/data" rendezvous-server
#
# The server is IPv6-only. Clients on IPv4-only networks are considered
# to have no Internet for Bitchat's purposes.
#
# The server generates its own Ed25519 identity on first run.
# The identity file is persisted via a volume mount so it survives
# container restarts. Share the server's public key with agents that
# should use it as a rendezvous point.

set -euo pipefail

PROJECT_ID="${1:-bitchat-anchor}"
REGION="${2:-us-central1}"
ZONE="${3:-us-central1-a}"
IMAGE_NAME="rendezvous-server"
VM_NAME="glp-rendezvous"
REPO_NAME="bitchat"
IPV6_PORT=9516
NETWORK="glp-vpc"
FIREWALL_RULE_IPV6="${NETWORK}-allow-bitchat-udp-ipv6"
SUBNET="glp-subnet-${REGION}"
# Region-specific non-overlapping CIDR so multiple regions can coexist in the VPC.
case "$REGION" in
  us-central1) SUBNET_RANGE="10.20.0.0/24" ;;
  us-east1)    SUBNET_RANGE="10.21.0.0/24" ;;
  us-west1)    SUBNET_RANGE="10.22.0.0/24" ;;
  me-west1)    SUBNET_RANGE="10.23.0.0/24" ;;
  *)           SUBNET_RANGE="10.20.0.0/24" ;;
esac

echo "=== GLP Rendezvous Server Deployment ==="
echo "Project: $PROJECT_ID"
echo "Region:  $REGION"
echo "Zone:    $ZONE"
echo ""

# 1. Create Artifact Registry repo (if not exists)
echo "--- Creating Artifact Registry repo ---"
gcloud artifacts repositories create "$REPO_NAME" \
  --repository-format=docker \
  --location="$REGION" \
  --project="$PROJECT_ID" \
  --quiet 2>/dev/null || echo "Repo already exists"

# 1b. Create custom-mode VPC with dual-stack subnet (GCE needs IPv4 for
# internal plumbing even though we only expose the server over IPv6).
# Auto-mode networks don't support IPv6 subnets, so we need a custom VPC.
echo "--- Ensuring custom VPC and dual-stack subnet ---"
gcloud compute networks create "$NETWORK" \
  --subnet-mode=custom \
  --project="$PROJECT_ID" \
  --quiet 2>/dev/null || echo "Network already exists"

if gcloud compute networks subnets describe "$SUBNET" \
    --region="$REGION" --project="$PROJECT_ID" >/dev/null 2>&1; then
  echo "Subnet already exists"
else
  gcloud compute networks subnets create "$SUBNET" \
    --network="$NETWORK" \
    --region="$REGION" \
    --range="$SUBNET_RANGE" \
    --stack-type=IPV4_IPV6 \
    --ipv6-access-type=EXTERNAL \
    --project="$PROJECT_ID" \
    --quiet
fi

# Allow SSH (IAP and direct) into the VPC so we can reach the VM for logs.
gcloud compute firewall-rules create "${NETWORK}-allow-ssh" \
  --network="$NETWORK" \
  --project="$PROJECT_ID" \
  --direction=INGRESS \
  --action=ALLOW \
  --rules=tcp:22 \
  --source-ranges=0.0.0.0/0 \
  --quiet 2>/dev/null || echo "SSH firewall rule already exists"

# 2. Build & push Docker image
#
# Build context is the bootstrap_anchor directory itself — grassroots_dart_udx
# is pulled from pub.dev during `dart pub get`, so no sibling source tree
# needs to be in the context.
#
# --platform linux/amd64 ensures the binary works on x86_64 GCE VMs even
# when building on Apple Silicon. `docker buildx build --push` is used
# instead of `docker build` + `docker push` so the amd64 image is shipped
# straight to the registry without being loaded into the local arm64
# daemon (which can't run it anyway).
#
# The default buildx builder on Docker Desktop uses the `docker` driver,
# which doesn't route amd64 RUN commands through Rosetta on Apple Silicon
# (RUN fails with `exec format error`). We provision a dedicated
# `docker-container` driver builder, which spawns its own BuildKit
# container that does honor Rosetta translation.
echo "--- Authenticating Docker against Artifact Registry ---"
gcloud auth configure-docker "${REGION}-docker.pkg.dev" --quiet

BUILDER=grassroots-builder
if ! docker buildx inspect "$BUILDER" >/dev/null 2>&1; then
  echo "--- Creating buildx builder ($BUILDER, docker-container driver) ---"
  docker buildx create --name "$BUILDER" --driver docker-container --bootstrap
fi

echo "--- Building & pushing image ---"
cd "$(dirname "$0")"
docker buildx build \
  --builder "$BUILDER" \
  --platform linux/amd64 \
  --push \
  -t "${REGION}-docker.pkg.dev/${PROJECT_ID}/${REPO_NAME}/${IMAGE_NAME}:latest" \
  .

# 3. Create firewall rule for IPv6 UDP.
echo "--- Creating firewall rule ---"
if gcloud compute firewall-rules describe "$FIREWALL_RULE_IPV6" \
    --project="$PROJECT_ID" >/dev/null 2>&1; then
  gcloud compute firewall-rules update "$FIREWALL_RULE_IPV6" \
    --project="$PROJECT_ID" \
    --allow="udp:${IPV6_PORT}" \
    --source-ranges=::/0 \
    --target-tags=glp-rendezvous \
    --quiet
else
  gcloud compute firewall-rules create "$FIREWALL_RULE_IPV6" \
    --network="$NETWORK" \
    --project="$PROJECT_ID" \
    --direction=INGRESS \
    --action=ALLOW \
    --rules=udp:${IPV6_PORT} \
    --source-ranges=::/0 \
    --target-tags=glp-rendezvous \
    --quiet
fi

# 4. Reserve static IPv6 address (idempotent — skips if it already exists).
#    Static IPv6 is required for reliable inbound routing on GCE; ephemeral
#    IPv6 addresses may not receive unsolicited inbound traffic in some zones.
IPV6_ADDR_NAME="${VM_NAME}-ipv6"
echo "--- Ensuring static IPv6 reservation ---"
if gcloud compute addresses describe "$IPV6_ADDR_NAME" \
    --region="$REGION" --project="$PROJECT_ID" >/dev/null 2>&1; then
  echo "Static IPv6 reservation already exists"
else
  gcloud compute addresses create "$IPV6_ADDR_NAME" \
    --region="$REGION" \
    --subnet="$SUBNET" \
    --ip-version=IPV6 \
    --endpoint-type=VM \
    --project="$PROJECT_ID" \
    --quiet
fi

# 5. Create VM (or update existing)
echo "--- Creating VM ---"
DATA_DISK="${VM_NAME}-data"

# COS ip6tables blocks inbound IPv6 by default even when GCE firewall rules
# allow the traffic. This startup script opens our port on every boot.
STARTUP_SCRIPT='#!/bin/bash
ip6tables -C INPUT -p udp --dport '"$IPV6_PORT"' -j ACCEPT 2>/dev/null || ip6tables -I INPUT 1 -p udp --dport '"$IPV6_PORT"' -j ACCEPT
ip6tables -C INPUT -p tcp --dport '"$IPV6_PORT"' -j ACCEPT 2>/dev/null || ip6tables -I INPUT 1 -p tcp --dport '"$IPV6_PORT"' -j ACCEPT
'

if gcloud compute instances describe "$VM_NAME" \
    --project="$PROJECT_ID" --zone="$ZONE" >/dev/null 2>&1; then
  echo "VM exists, updating container..."
  gcloud compute instances update-container "$VM_NAME" \
    --project="$PROJECT_ID" \
    --zone="$ZONE" \
    --container-image="${REGION}-docker.pkg.dev/${PROJECT_ID}/${REPO_NAME}/${IMAGE_NAME}:latest" \
    --container-arg="--identity" --container-arg="/app/data/identity.json" \
    --container-arg="--ipv6-port" --container-arg="$IPV6_PORT" \
    --quiet
else
  echo "Creating new VM..."
  # Separate persistent data disk so identity.json survives VM delete/recreate.
  # auto-delete=no keeps the disk even if the VM is deleted.
  #
  # gcloud create-with-container has a bug: --network-interface does not
  # propagate network-tier to ipv6AccessConfigs, so we create without IPv6
  # first, then attach it in a separate step below.
  gcloud compute instances create-with-container "$VM_NAME" \
    --project="$PROJECT_ID" \
    --zone="$ZONE" \
    --machine-type=e2-micro \
    --tags=glp-rendezvous \
    --network-interface="network=${NETWORK},subnet=${SUBNET},stack-type=IPV4_IPV6" \
    --container-image="${REGION}-docker.pkg.dev/${PROJECT_ID}/${REPO_NAME}/${IMAGE_NAME}:latest" \
    --container-arg="--identity" --container-arg="/app/data/identity.json" \
    --container-arg="--ipv6-port" --container-arg="$IPV6_PORT" \
    --create-disk="name=${DATA_DISK},size=1GB,type=pd-standard,auto-delete=no,device-name=${DATA_DISK}" \
    --container-mount-disk="mount-path=/app/data,name=${DATA_DISK}" \
    --boot-disk-size=10GB \
    --metadata=google-logging-enabled=true,startup-script="$STARTUP_SCRIPT" \
    --quiet

  # Attach static IPv6: stop → update NIC → start.
  echo "--- Attaching static IPv6 ---"
  gcloud compute instances stop "$VM_NAME" \
    --zone="$ZONE" --project="$PROJECT_ID" --quiet

  gcloud compute instances network-interfaces update "$VM_NAME" \
    --zone="$ZONE" --project="$PROJECT_ID" \
    --network-interface=nic0 \
    --ipv6-network-tier=PREMIUM \
    --external-ipv6-address="$IPV6_ADDR_NAME" \
    --external-ipv6-prefix-length=96

  gcloud compute instances start "$VM_NAME" \
    --zone="$ZONE" --project="$PROJECT_ID" --quiet
fi

# 6. Report
echo ""
echo "--- Deployment complete ---"
echo ""
IPV6=$(gcloud compute instances describe "$VM_NAME" \
  --project="$PROJECT_ID" --zone="$ZONE" \
  --format='get(networkInterfaces[0].ipv6AccessConfigs[0].externalIpv6)' 2>/dev/null)

if [ -n "${IPV6:-}" ]; then
  echo "Rendezvous IPv6 address (for clients): [${IPV6}]:${IPV6_PORT}"
else
  echo "WARNING: no external IPv6 attached to $VM_NAME" >&2
fi
echo ""
echo "Next steps:"
echo "  1. SSH into the VM and check the logs for the server's public key"
echo "     gcloud compute ssh $VM_NAME --zone=$ZONE -- docker logs \$(docker ps -q)"
echo "  2. Share the public key and address with agents"
echo "  3. On each phone, go to Settings → Rendezvous Server"
echo "  4. Enter the IPv6 server address plus the server public key"
