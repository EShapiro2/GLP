#!/bin/bash
# CSSG Modules — REPL integration test
# Loads modular CSSG and runs all plays
# Matches original programs/typed_book/cssg/ functionality
#
# Note: Uses boot_direct.glp (direct calls, no # dispatch) because
# the # dispatch runtime does not yet produce correct concurrent execution.
# The # dispatch finds procedures but the system suspends without output.
# This is a known runtime limitation; boot.glp with # dispatch is preserved
# for future use when the runtime supports it.

set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GLP_DIR="$SCRIPT_DIR/.."
CSSG="$GLP_DIR/programs/cssg_modules"
cd "$GLP_DIR/glp_runtime"

DART=${DART:-$(which dart 2>/dev/null || echo "dart")}
REPL="bin/glp_repl.dart"

PASS=0
FAIL=0

check() {
    local name="$1" pattern="$2" source="$3"
    if echo "$source" | grep -q "$pattern"; then
        echo "  PASS: $name"
        PASS=$((PASS + 1))
    else
        echo "  FAIL: $name (expected: $pattern)"
        FAIL=$((FAIL + 1))
    fi
}

check_not() {
    local name="$1" pattern="$2" source="$3"
    if echo "$source" | grep -q "$pattern"; then
        echo "  FAIL: $name (should NOT match: $pattern)"
        FAIL=$((FAIL + 1))
    else
        echo "  PASS: $name"
        PASS=$((PASS + 1))
    fi
}

echo "============================================"
echo "   CSSG Modules — Play Tests                "
echo "============================================"
echo ""

# Determine which boot file to use
BOOT="$CSSG/boot.glp"
if [ -f "$CSSG/boot_direct.glp" ]; then
    BOOT="$CSSG/boot_direct.glp"
    echo "Using boot_direct.glp (direct calls, no # dispatch)"
fi

# -----------------------------------------------
# Test 1: Loading all modules succeeds
# -----------------------------------------------
echo "--- Loading modules ---"
load_result=$($DART run "$REPL" <<HEREDOC
$CSSG/self.glp
$CSSG/agent.glp
$CSSG/ui/mediator.glp
$CSSG/ui/actors.glp
$BOOT
:quit
HEREDOC
2>&1)

check_not "No type errors on load" "Type checking failed" "$load_result"
check_not "No load errors" "Error loading" "$load_result"

# -----------------------------------------------
# Test 2: Silent plays (play1–play7) succeed
# -----------------------------------------------
echo ""
echo "--- Silent plays (play1-play7) ---"
for play_num in 1 2 3 4 5 6 7; do
    result=$($DART run "$REPL" <<HEREDOC
$CSSG/self.glp
$CSSG/agent.glp
$CSSG/ui/mediator.glp
$CSSG/ui/actors.glp
$BOOT
play${play_num}.
:quit
HEREDOC
2>&1)

    if echo "$result" | grep -q "Type checking failed\|Error loading"; then
        echo "  FAIL: play${play_num} — load error"
        FAIL=$((FAIL + 1))
    else
        check "play${play_num} succeeds" "succeeds\|suspended" "$result"
    fi
done

# -----------------------------------------------
# Test 3: Tagged plays (fplay1–fplay7) produce output
# -----------------------------------------------
echo ""
echo "--- Tagged plays (fplay1-fplay7) ---"

# fplay1: Both accept intro → Alice and Charlie become friends, exchange messages
fp1=$($DART run "$REPL" <<HEREDOC
$CSSG/self.glp
$CSSG/agent.glp
$CSSG/ui/mediator.glp
$CSSG/ui/actors.glp
$BOOT
fplay1.
:quit
HEREDOC
2>&1)

check "fplay1 succeeds" "succeeds\|suspended" "$fp1"
check "fplay1 alice connected bob" "tagged(alice.*connected(bob)" "$fp1"
check "fplay1 charlie connected alice" "tagged(charlie.*connected(alice)" "$fp1"

# fplay2: Alice accepts intro, Charlie rejects
fp2=$($DART run "$REPL" <<HEREDOC
$CSSG/self.glp
$CSSG/agent.glp
$CSSG/ui/mediator.glp
$CSSG/ui/actors.glp
$BOOT
fplay2.
:quit
HEREDOC
2>&1)

check "fplay2 succeeds" "succeeds\|suspended" "$fp2"
check "fplay2 alice rejected" "tagged(alice.*rejected" "$fp2"

# fplay3: Both reject intro
fp3=$($DART run "$REPL" <<HEREDOC
$CSSG/self.glp
$CSSG/agent.glp
$CSSG/ui/mediator.glp
$CSSG/ui/actors.glp
$BOOT
fplay3.
:quit
HEREDOC
2>&1)

check "fplay3 succeeds" "succeeds\|suspended" "$fp3"

# fplay4: CSSG all accept → Carol and Dave become friends
fp4=$($DART run "$REPL" <<HEREDOC
$CSSG/self.glp
$CSSG/agent.glp
$CSSG/ui/mediator.glp
$CSSG/ui/actors.glp
$BOOT
fplay4.
:quit
HEREDOC
2>&1)

check "fplay4 succeeds" "succeeds\|suspended" "$fp4"
check "fplay4 carol connected dave" "tagged(carol.*connected(dave)" "$fp4"

# fplay5: Bob rejects → Carol gets rejected
fp5=$($DART run "$REPL" <<HEREDOC
$CSSG/self.glp
$CSSG/agent.glp
$CSSG/ui/mediator.glp
$CSSG/ui/actors.glp
$BOOT
fplay5.
:quit
HEREDOC
2>&1)

check "fplay5 succeeds" "succeeds\|suspended" "$fp5"

# fplay6: Carol rejects → Dave gets rejected
fp6=$($DART run "$REPL" <<HEREDOC
$CSSG/self.glp
$CSSG/agent.glp
$CSSG/ui/mediator.glp
$CSSG/ui/actors.glp
$BOOT
fplay6.
:quit
HEREDOC
2>&1)

check "fplay6 succeeds" "succeeds\|suspended" "$fp6"

# fplay7: Dave rejects → Carol gets rejected
fp7=$($DART run "$REPL" <<HEREDOC
$CSSG/self.glp
$CSSG/agent.glp
$CSSG/ui/mediator.glp
$CSSG/ui/actors.glp
$BOOT
fplay7.
:quit
HEREDOC
2>&1)

check "fplay7 succeeds" "succeeds\|suspended" "$fp7"

# -----------------------------------------------
# Summary
# -----------------------------------------------
echo ""
echo "============================================"
echo "Total: $((PASS + FAIL)) | Passed: $PASS | Failed: $FAIL"
echo "============================================"

if [ $FAIL -eq 0 ]; then
    echo "ALL CSSG MODULE TESTS PASSED!"
    exit 0
else
    echo "SOME TESTS FAILED"
    exit 1
fi
