# Dotty Folder Symlinks - Technical Requirements

## Executive Summary

This technical specification defines the implementation approach for extending the dotty module to support opt-in directory-level symlinks. The solution maintains backward compatibility while adding a new `link_directory` boolean column to the configuration schema. When enabled, entire directories will be symlinked as single entities rather than recursively linking individual files.

## Architecture Overview

### System Context

The dotty module currently processes symlinks through a file-based approach:

1. Configuration defines source → target mappings
2. Files are discovered recursively using glob patterns
3. Individual symlinks are created for each file
4. A cache tracks all linked files for cleanup

The new feature adds a parallel path for directory-level symlinks while reusing existing infrastructure for configuration, caching, and conflict detection.

### Component Architecture

```
config.nu (extended)
    ↓ [load configuration with link_directory column]
mod.nu::link
    ↓ [route based on link_directory flag]
    ├→ get-project-files-to-link (existing path for files)
    └→ get-project-dirs-to-link (new path for directories)
         ↓
    assert-no-conflicts (extended for directory awareness)
         ↓
    [parallel symlink creation]
         ↓
    cache.nu (stores paths - directories or files)
```

### Data Architecture

Configuration schema extension:

- Add optional `link_directory: bool` column (defaults to false)
- When true, `real` and `symlink` paths represent directories
- Cache format remains unchanged (list of path strings)
- Conflict detection extended to handle directory/file overlaps

## Technology Decisions

### Framework/Library Selection

| Requirement        | Option 1                     | Option 2                      | Recommendation                                 |
| ------------------ | ---------------------------- | ----------------------------- | ---------------------------------------------- |
| Schema extension   | Optional column with default | Required column               | **Option 1**: Maintains backward compatibility |
| Conflict detection | Extend existing function     | New separate function         | **Option 1**: Simpler, reuses validation logic |
| Directory symlink  | Native `ln -sf`              | Custom nushell implementation | **Option 1**: Standard, reliable, atomic       |
| Cache format       | Unified format               | Separate directory cache      | **Option 1**: Simpler, less code duplication   |

### Integration Patterns

- Configuration loading remains synchronous
- Parallel processing maintained for multiple projects
- Atomic directory symlink operations (single `ln -sf` call)
- Early validation before any filesystem modifications

## Implementation Tasks

### Component: Configuration Schema Extension

Location: `config/nushell/scripts/ct/dotty`

- [ ] **CONFIG-1**: Extend configuration table type definition (delivers FR-1, NFR-2)
  - Updates: `config/nushell/scripts/ct/dotty/config.nu:4:28`
  - Change table type to include optional `link_directory: bool` column
  - Ensure backward compatibility with existing configurations

- [ ] **CONFIG-2**: Update configuration examples (delivers FR-1)
  - Updates: `config/nushell/scripts/ct/dotty/config.nu:11:22`
  - Add claude project with `link_directory: true`
  - Update type annotation in export def

- [ ] **CONFIG-3**: Fix dir helper function to not follow symlinks (delivers FR-3)
  - Updates: `config/nushell/scripts/ct/dotty/config.nu:32:34`
  - Add `-n` flag to `path expand` in dir helper function
  - Prevents existing symlinks from being resolved to their targets
  - Ensures configuration paths remain as intended

### Component: Directory Symlink Processing

Location: `config/nushell/scripts/ct/dotty`

- [ ] **LINK-1**: Create get-project-dirs-to-link function (delivers FR-2)
  - Creates: `config/nushell/scripts/ct/dotty/mod.nu:115:1`
  - Function to handle directory-based project processing
  - Return simplified structure with single directory path
  - Skip file discovery entirely

- [ ] **LINK-2**: Modify main link command routing (delivers FR-2, FR-5)
  - Updates: `config/nushell/scripts/ct/dotty/mod.nu:22:24`
  - Check `link_directory` flag for each project
  - Route to appropriate processing function
  - Emit warning if excludes specified for directory symlinks

- [ ] **LINK-3**: Implement directory symlink creation (delivers FR-2, FR-4)
  - Updates: `config/nushell/scripts/ct/dotty/mod.nu:32:37`
  - Handle directory symlink creation in parallel block
  - Use `ln -sf` for atomic directory symlink
  - Delete existing target if present (respecting --force flag)

### Component: Conflict Detection Enhancement

Location: `config/nushell/scripts/ct/dotty`

- [ ] **CONFLICT-1**: Add path overlap detection (delivers FR-3)
  - Creates: `config/nushell/scripts/ct/dotty/helpers.nu:48:1`
  - Function to detect overlapping paths (e.g., foo vs foo/bar)
  - Check parent/child relationships between paths
  - Return clear error messages with conflicting entries
  - **IMPORTANT**: Only flag conflicts when at least one config has `link_directory: true`
  - File-based configs (link_directory: false) can safely overlap as they only link their source files

- [ ] **CONFLICT-2**: Integrate overlap detection into assert-no-conflicts (delivers FR-3, NFR-3)
  - Updates: `config/nushell/scripts/ct/dotty/helpers.nu:2:37`
  - Call overlap detection before existing checks
  - Validate directory vs file configurations don't conflict
  - Ensure all validation occurs before filesystem changes

- [ ] **CONFLICT-3**: Handle directory symlink conflicts (delivers FR-3)
  - Updates: `config/nushell/scripts/ct/dotty/helpers.nu:39:45`
  - Extend is-symlink to properly detect directory symlinks
  - Handle force flag for directory symlink overwrites
  - Provide clear user prompts for directory conflicts

- [ ] **CONFLICT-4**: Prevent symlink path expansion during conflict checks (delivers FR-3)
  - Updates: `config/nushell/scripts/ct/dotty/helpers.nu:59:61`
  - Use `path expand -n` flag to avoid following existing symlinks
  - Ensures conflict detection uses intended paths, not resolved symlink targets
  - Critical for correct overlap detection when symlinks already exist

### Component: Cache Management Updates

Location: `config/nushell/scripts/ct/dotty`

- [ ] **CACHE-1**: Update cache storage for directory symlinks (delivers FR-4)
  - Updates: `config/nushell/scripts/ct/dotty/mod.nu:39:42`
  - Store single directory path when link_directory is true
  - Maintain compatibility with existing file-based cache entries
  - Sort and save updated cache consistently

- [ ] **CACHE-2**: Handle cache transitions (delivers FR-4, NFR-2)
  - Updates: `config/nushell/scripts/ct/dotty/mod.nu:22:24`
  - Detect when configuration changes from file to directory mode
  - Clean up old file-based entries when switching to directory
  - Preserve cache integrity during mode transitions

### Component: Command Extensions

Location: `config/nushell/scripts/ct/dotty`

- [ ] **CMD-1**: Update teardown command for directory symlinks (delivers AC-7)
  - Updates: `config/nushell/scripts/ct/dotty/mod.nu:150:1`
  - Detect and remove directory symlinks correctly
  - Ensure only symlink removed, not source contents
  - Handle mixed file/directory configurations

- [ ] **CMD-2**: Extend prune command for directory symlinks (delivers AC-8, FR-4)
  - Updates: `config/nushell/scripts/ct/dotty/mod.nu:89:98`
  - Detect orphaned directory symlinks
  - Remove broken directory symlinks safely
  - Report pruned directory symlinks to user

## Technical Specifications

### API Design

No external API changes - the feature is accessed through configuration only.

### Configuration Schema

```nushell
# Extended table type
table<
  name: string,
  real: path,
  symlink: path,
  excludes: list<path>,
  link_directory: bool  # New optional column
>

# Example usage
[
  [name, real, symlink, excludes, link_directory];
  [claude, (dir ~/PersonalConfigs/claude), (dir ~/.claude), [], true]
  [home, (dir ~/PersonalConfigs/home), (dir ~), ["**/.stylua.toml"], false]
]
```

### Validation Logic

```nushell
# Path overlap detection pseudocode
def detect-overlaps [configs] {
  for config in configs {
    for other in configs {
      if config != other {
        if is-parent-path config.symlink other.symlink {
          error "Path overlap detected"
        }
      }
    }
  }
}
```

## Testing Strategy

**IMPORTANT:** at this time, config is hardcoded, so comment out existing config and add test config during testing.

### Unit Testing

Manual testing scenarios for each component:

1. Directory symlink creation with clean target
2. Directory symlink with existing file at target
3. Directory symlink with existing directory at target
4. Mixed configuration (files and directories)
5. Conflict detection for overlapping paths
6. Cache transitions between modes

### Integration Testing

End-to-end scenarios:

1. Fresh install with directory symlinks
2. Migration from file to directory mode
3. Teardown and recreation cycles
4. Prune operation on mixed configurations

## Operational Considerations

### Error Handling

- Clear error messages for configuration conflicts

## Technical Debt Considerations

- Current implementation mixes concerns in mod.nu - consider future refactoring
- Cache format could be enhanced with metadata
- Dry-run mode would improve safety (noted in spec)

## Dependencies and Prerequisites

- Nushell environment (existing requirement)
- Standard Unix ln command (existing requirement)
- File system permissions for symlink creation (existing requirement)

## Implementation Sequence

Recommended order to minimize risk:

1. CONFIG-1, CONFIG-2 - Non-breaking schema extension
2. CONFLICT-1, CONFLICT-2, CONFLICT-3 - Safety validations
3. LINK-1, LINK-2, LINK-3 - Core functionality
4. CACHE-1, CACHE-2 - State management
5. CMD-1, CMD-2 - Command enhancements

This ensures safety mechanisms are in place before any directory symlink operations are possible.
