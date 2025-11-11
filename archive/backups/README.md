# GLP Backups Directory

This directory contains archived and backup versions of the GLP codebase.

## Contents

- **OLD/** - Old code from before refactoring
- **refactoring/** - Refactoring attempts and experiments
- **20251110_HHMM_clean_state/** - Clean state snapshot after reorganization
- **glp_backup_20251110_0951.tar.gz** - Full compressed backup before reorganization (contains both glp_runtime/ and udi/ directories)

## Restore Instructions

To restore from the full backup:
```bash
cd /Users/udi/GLP
tar -xzf glp_backups/glp_backup_20251110_0951.tar.gz
```

To compare with clean state:
```bash
diff -r glp_runtime glp_backups/20251110_HHMM_clean_state
```

## Notes

- All backups created: November 10, 2025
- Reorganization removed duplicate test/ directory from glp_runtime
- Tests are now only in /Users/udi/GLP/glp_tests/
- Active code is in /Users/udi/GLP/glp_runtime/
