# Dotty Config Field Rename

User Sign off: codethread

## Context and Problem Statement

The dotty symlink management module currently uses field names 'real' and 'symlink' in its configuration structure. These names should be updated to 'origin' and 'target' for better semantic clarity and consistency with standard symlink terminology.

## Value Statement

This change will:

- Improve code readability by using more standard terminology
- Make the configuration more intuitive for new users
- Align with common symlink naming conventions where 'origin' is the source and 'target' is the destination

## Stakeholders

- **Users**: Developers using the dotty module for managing symlinks
- **Maintainers**: PersonalConfigs maintainers
- **Dependencies**: Any scripts or tools that parse dotty configuration files

## Technical Architecture

The dotty module consists of:

- Nushell scripts at `config/nushell/scripts/ct/dotty/`
- TOML configuration files at `config/dotty/`
- Core functionality for managing symlinks between PersonalConfigs and the home directory

## Functional Requirements

### FR-1: Rename Config Fields

The system SHALL rename all instances of the 'real' field to 'origin' and 'symlink' field to 'target' throughout the codebase.

### FR-2: Update Configuration Files

The system SHALL update all existing TOML configuration files to use the new field names.

### FR-3: Maintain Backward Compatibility

The system SHALL ensure all existing functionality continues to work with the new field names.

## Non-Functional Requirements

### NFR-1: Code Quality

All changes SHALL maintain the existing code style and conventions of the project.

### NFR-2: Testing

The renamed configuration SHALL be tested to ensure symlink creation, removal, and management work correctly.

### NFR-3: Documentation

Any inline documentation or comments referencing these fields SHALL be updated to reflect the new names.

## External Dependencies

- Nushell scripting environment
- TOML configuration file format

## Interface Definitions

### Configuration Schema

**Before:**

```toml
[[project]]
name = "example"
real = "~/PersonalConfigs/home"
symlink = "~"
```

**After:**

```toml
[[project]]
name = "example"
origin = "~/PersonalConfigs/home"
target = "~"
```

## Acceptance Criteria

1. All references to 'real' field renamed to 'origin' in:
   - `/Users/codethread/PersonalConfigs/config/nushell/scripts/ct/dotty/config.nu`
   - `/Users/codethread/PersonalConfigs/config/nushell/scripts/ct/dotty/mod.nu`
   - `/Users/codethread/PersonalConfigs/config/nushell/scripts/ct/dotty/helpers.nu`

2. All references to 'symlink' field renamed to 'target' in the same files

3. Configuration files updated:
   - `/Users/codethread/PersonalConfigs/config/dotty/dotty.toml`
   - `/Users/codethread/PersonalConfigs/config/dotty/test-dotty.toml`

4. All dotty commands (`dotty sync`, `dotty clean`, `dotty test`) work correctly with new field names

## Implementation Notes

- The table type signature in `config.nu` needs updating
- Field validation error messages need updating
- All path expansion and validation logic must use the new field names
- Cache structures may need updating if they reference these fields

## Technical Debt Tracking

None identified - this is a straightforward refactoring with no shortcuts needed.
