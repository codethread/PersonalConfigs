# Dotty Folder Symlinks

## Context and Problem Statement

The current dotty module recursively traverses directories and creates individual file symlinks. This approach, while comprehensive, adds unnecessary complexity when entire directories need to be symlinked as-is. Users need an opt-in mechanism to symlink folders directly rather than their individual contents, particularly for directories like `~/.claude/` where bidirectional syncing of all contents is desired.

## Value Statement

This feature will:

- Simplify configuration for directories that should always be fully synced
- Reduce processing overhead by avoiding recursive file traversal for specified folders
- Enable cleaner bidirectional syncing between source and target locations
- Maintain backward compatibility with existing file-based symlink behavior

## Stakeholders

- **Users**: Developers using PersonalConfigs for dotfile management
- **Maintainers**: PersonalConfigs repository maintainers
- **Dependencies**: No external dependencies; affects only the dotty module

## Technical Architecture

The solution extends the existing dotty module configuration table with a new `link_directory` column. When set to `true`, the entire directory is symlinked directly rather than traversing its contents. The implementation maintains separation between directory-based and file-based linking logic while reusing existing infrastructure for caching, conflict detection, and cleanup.

## Functional Requirements

### FR-1: Configuration Extension

The system SHALL support a new `link_directory` boolean column in the dotty configuration table that:

- Defaults to `false` to maintain backward compatibility
- When set to `true`, treats the configured path as a single directory to symlink
- Is mutually exclusive with file-based traversal for the same configuration entry

### FR-2: Directory Symlink Creation

When `link_directory` is `true`, the system SHALL:

- Create a single symlink from the source directory to the target location
- Skip recursive file traversal entirely for that configuration entry
- Delete any existing symlink or file at the target location before creating the directory symlink
- Apply `--force` flag behavior consistently with file-based symlinks

### FR-3: Conflict Detection

The system SHALL detect and prevent conflicts during configuration parsing when:

- Multiple configurations target the same symlink location
- A directory symlink configuration overlaps with a file-based configuration (e.g., `foo/bar` as directory and `foo/bar/baz` as files)
- Conflicts SHALL be reported before any filesystem modifications occur based on common path bases
- Error messages SHALL clearly identify the conflicting configuration entries

### FR-4: Cache Management

The system SHALL:

- Store only the directory path in the cache when `link_directory` is `true`
- Handle cache updates for directory symlinks separately from file symlinks
- Support the existing `dotty prune` command for directory symlinks

### FR-5: Excludes Handling

When `link_directory` is `true`, the system SHALL:

- Ignore any values in the `excludes` column
- Emit a warning if `excludes` are specified for a directory symlink configuration
- Document that `excludes` are meaningless for directory symlinks

## Non-Functional Requirements

### NFR-1: Performance

The system SHALL maintain or improve performance compared to recursive file traversal for large directories

### NFR-2: Backward Compatibility

The system SHALL:

- Continue to support all existing configurations without modification
- Default to file-based symlinks when `link_directory` is not specified
- Maintain the existing API for the `dotty link` command

### NFR-3: Safety

The system SHALL:

- Never modify files without explicit user confirmation (unless `--force` is used)
- Provide clear warnings when directory symlinks would overwrite existing content
- Validate configuration integrity before making any filesystem changes

### NFR-4: Maintainability

The implementation SHALL:

- Follow existing nushell patterns in the codebase
- Use parallel operations where beneficial but prioritize simplicity
- Maintain clear separation between directory and file symlink logic

## External Dependencies

None - the feature is self-contained within the dotty module

## Interface Definitions

### Configuration Table Schema

```nushell
table<
  name: string,
  real: path,
  symlink: path,
  excludes: list<path>,
  link_directory: bool  # New optional column, defaults to false
>
```

### Example Configuration

```nushell
[
  [name, real, symlink, excludes, link_directory];
  [claude, (dir ~/PersonalConfigs/claude), (dir ~/.claude), [], true]  # Directory symlink
  [home, (dir ~/PersonalConfigs/home), (dir ~), [ "**/.stylua.toml" ], false]  # File symlinks (default)
]
```

## Acceptance Criteria

1. ✓ Configuration with `link_directory: true` creates a single directory symlink
2. ✓ Configuration without `link_directory` or with `link_directory: false` maintains current behavior
3. ✓ Conflicting configurations are detected during parsing before any filesystem changes
4. ✓ Cache correctly tracks directory symlinks
5. ✓ `--force` flag works consistently for directory symlinks
6. ✓ Warnings are displayed when `excludes` are specified for directory symlinks
7. ✓ `dotty teardown` removes directory symlinks correctly (only the symlink, not source contents)
8. ✓ `dotty prune` handles orphaned directory symlinks

## Implementation Notes

- The implementation should prioritize simplicity over performance optimization
- Separate code paths for directory vs file symlinks will improve maintainability
- Early validation in `config load` to detect conflicts based on common path bases
- The cache format can remain unchanged (storing paths as strings)

## Technical Debt Tracking

- Future enhancement: Consider supporting nested directory symlinks with conflict resolution strategies
- Future enhancement: Add dry-run mode to preview symlink operations
- Documentation: Update README with directory symlink examples and best practices
