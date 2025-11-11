#!/bin/bash
# Package files for sharing with Claude Web
# Usage: ./package_for_claude_web.sh [minimal|full]

MODE="${1:-minimal}"
OUTPUT_DIR="$HOME/Downloads"
TIMESTAMP=$(date +%Y%m%d_%H%M)

cd "$(dirname "$0")"

case "$MODE" in
  minimal)
    echo "Creating minimal package for Claude Web..."
    OUTPUT="$OUTPUT_DIR/glp_minimal_$TIMESTAMP.zip"
    zip -r "$OUTPUT" \
      CLAUDE.md \
      SPEC_GUIDE.md \
      docs/glp-bytecode-v216-complete.md \
      docs/glp-runtime-spec.txt \
      -x "*.git*" "*.DS_Store"
    ;;

  full)
    echo "Creating full context package for Claude Web..."
    OUTPUT="$OUTPUT_DIR/glp_full_$TIMESTAMP.zip"
    zip -r "$OUTPUT" \
      CLAUDE.md \
      SPEC_GUIDE.md \
      docs/ \
      glp_runtime/lib/ \
      glp_runtime/pubspec.yaml \
      glp_runtime/analysis_options.yaml \
      -x "*.git*" "*.DS_Store" "*/.dart_tool/*" "*/build/*"
    ;;

  runtime)
    echo "Creating runtime source package for Claude Web..."
    OUTPUT="$OUTPUT_DIR/glp_runtime_$TIMESTAMP.zip"
    zip -r "$OUTPUT" \
      CLAUDE.md \
      glp_runtime/lib/bytecode/ \
      glp_runtime/lib/compiler/ \
      glp_runtime/lib/runtime/ \
      -x "*.git*" "*.DS_Store"
    ;;

  *)
    echo "Usage: $0 [minimal|full|runtime]"
    echo ""
    echo "  minimal  - CLAUDE.md + key specs (for quick questions)"
    echo "  full     - CLAUDE.md + all docs + runtime lib (for architecture)"
    echo "  runtime  - CLAUDE.md + runtime source only (for implementation)"
    exit 1
    ;;
esac

echo "Package created: $OUTPUT"
ls -lh "$OUTPUT"
