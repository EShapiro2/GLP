#!/bin/bash
#
# Tear down all GCP resources created by deploy.sh.
# Run this before deploy.sh to get a clean slate.
#
# Usage:
#   ./teardown.sh [PROJECT_ID] [REGION] [ZONE]

set -euo pipefail

PROJECT_ID="${1:-bitchat-anchor}"
REGION="${2:-us-east5}"
ZONE="${3:-us-east5-a}"
VM_NAME="glp-rendezvous"
REPO_NAME="bitchat"
NETWORK="glp-vpc"
SUBNET="glp-subnet-${REGION}"
IPV6_ADDR_NAME="${VM_NAME}-ipv6"
DATA_DISK="${VM_NAME}-data"

echo "=== GLP Rendezvous Server Teardown ==="
echo "Project: $PROJECT_ID"
echo "Region:  $REGION"
echo "Zone:    $ZONE"
echo ""

# 1. Delete VM (and boot disk, but keep data disk for confirmation)
if gcloud compute instances describe "$VM_NAME" \
    --zone="$ZONE" --project="$PROJECT_ID" >/dev/null 2>&1; then
  echo "--- Deleting VM $VM_NAME ---"
  gcloud compute instances delete "$VM_NAME" \
    --zone="$ZONE" --project="$PROJECT_ID" \
    --delete-disks=boot --quiet
else
  echo "VM $VM_NAME not found, skipping"
fi

# 2. Delete data disk
if gcloud compute disks describe "$DATA_DISK" \
    --zone="$ZONE" --project="$PROJECT_ID" >/dev/null 2>&1; then
  echo "--- Deleting data disk $DATA_DISK ---"
  gcloud compute disks delete "$DATA_DISK" \
    --zone="$ZONE" --project="$PROJECT_ID" --quiet
else
  echo "Data disk $DATA_DISK not found, skipping"
fi

# 3. Delete all glp-rendezvous address reservations in this region
ADDRS=$(gcloud compute addresses list \
  --filter="name~'^${VM_NAME}' AND region:${REGION}" \
  --project="$PROJECT_ID" --format="value(name)" 2>/dev/null)
if [ -n "$ADDRS" ]; then
  for ADDR in $ADDRS; do
    echo "--- Deleting address reservation $ADDR ---"
    gcloud compute addresses delete "$ADDR" \
      --region="$REGION" --project="$PROJECT_ID" --quiet
  done
else
  echo "No address reservations matching ${VM_NAME}* in $REGION"
fi

# 4. Delete firewall rules
for RULE in "${NETWORK}-allow-bitchat-udp-ipv4" \
            "${NETWORK}-allow-bitchat-udp-ipv6" \
            "${NETWORK}-allow-ssh" \
            "${NETWORK}-test-ipv6-udp"; do
  if gcloud compute firewall-rules describe "$RULE" \
      --project="$PROJECT_ID" >/dev/null 2>&1; then
    echo "--- Deleting firewall rule $RULE ---"
    gcloud compute firewall-rules delete "$RULE" \
      --project="$PROJECT_ID" --quiet
  fi
done

# 5. Delete subnet
if gcloud compute networks subnets describe "$SUBNET" \
    --region="$REGION" --project="$PROJECT_ID" >/dev/null 2>&1; then
  echo "--- Deleting subnet $SUBNET ---"
  gcloud compute networks subnets delete "$SUBNET" \
    --region="$REGION" --project="$PROJECT_ID" --quiet
else
  echo "Subnet $SUBNET not found, skipping"
fi

# 6. Delete VPC (only if no subnets remain)
if gcloud compute networks describe "$NETWORK" \
    --project="$PROJECT_ID" >/dev/null 2>&1; then
  REMAINING=$(gcloud compute networks subnets list \
    --network="$NETWORK" --project="$PROJECT_ID" \
    --format="value(name)" 2>/dev/null | wc -l | tr -d ' ')
  if [ "$REMAINING" -eq 0 ]; then
    echo "--- Deleting VPC $NETWORK ---"
    gcloud compute networks delete "$NETWORK" \
      --project="$PROJECT_ID" --quiet
  else
    echo "VPC $NETWORK still has $REMAINING subnet(s) in other regions, skipping"
  fi
else
  echo "VPC $NETWORK not found, skipping"
fi

# 7. Delete Artifact Registry repo
if gcloud artifacts repositories describe "$REPO_NAME" \
    --location="$REGION" --project="$PROJECT_ID" >/dev/null 2>&1; then
  echo "--- Deleting Artifact Registry repo $REPO_NAME in $REGION ---"
  gcloud artifacts repositories delete "$REPO_NAME" \
    --location="$REGION" --project="$PROJECT_ID" --quiet
else
  echo "Artifact Registry repo $REPO_NAME in $REGION not found, skipping"
fi

echo ""
echo "=== Teardown complete ==="
echo "Run deploy.sh to recreate from scratch."
