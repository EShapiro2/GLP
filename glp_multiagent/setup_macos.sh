#!/bin/bash
# Setup script for GLP Multiagent on macOS
# Run from /Users/udi/GLP/glp_multiagent

set -e

echo "=== GLP Multiagent macOS Setup ==="
echo ""

# Check Flutter
if ! command -v flutter &> /dev/null; then
    echo "ERROR: Flutter not found. Please install Flutter first."
    echo "Visit: https://flutter.dev/docs/get-started/install/macos"
    exit 1
fi

# Check macOS desktop enabled
if ! flutter config | grep -q "enable-macos-desktop: true"; then
    echo "Enabling macOS desktop support..."
    flutter config --enable-macos-desktop
fi

# Initialize Flutter project (creates macos/ directory)
echo ""
echo "Initializing Flutter project..."
flutter create --platforms=macos .

# Get dependencies
echo ""
echo "Getting dependencies..."
flutter pub get

# Reminder about AppDelegate configuration
echo ""
echo "=== IMPORTANT: Manual Configuration Required ==="
echo ""
echo "Edit macos/Runner/AppDelegate.swift:"
echo "  - Change applicationShouldTerminateAfterLastWindowClosed to return false"
echo ""
echo "See README.md for full configuration details."
echo ""

# Try to run
echo "Attempting to run..."
flutter run -d macos
