# Technical Notes: Dotty Folder Symlinks

## Current Implementation Analysis

### File Processing Flow

1. `config load` returns table of projects
2. `get-project-files-to-link` for each project:
   - Changes to project directory (for gitignore)
   - Calls `list-files` which uses `glob **/*` with excludes
   - Filters through git check-ignore
   - Compares with cache to find new/deleted files
3. `assert-no-conflicts` validates no duplicate symlink targets
4. Creates symlinks in parallel
5. Updates cache

### Key Components

- **config.nu**: Defines project configurations
- **list-files.nu**: Handles file discovery with gitignore
- **cache.nu**: Manages state between runs
- **helpers.nu**: Conflict detection and symlink validation

## Implementation Considerations

### Configuration Schema Change

```nushell
# Current schema
table<name: string, real: path, symlink: path, excludes: list<path>>

# New schema
table<name: string, real: path, symlink: path, excludes: list<path>, link_directory: bool>
```

The `link_directory` column should be optional with default `false` behavior. Nushell tables support optional columns well.

### Conflict Detection Strategy

Need to check for path overlaps:

```nushell
# Example conflict scenarios:
# 1. Same target: foo -> /target, bar -> /target
# 2. Nested: foo/bar (folder) vs foo/bar/baz (files)
# 3. Parent/child: foo (folder) vs foo/bar (folder)
```

Can implement early validation in `config load` before any filesystem operations.

### Cache Implications

Current cache stores list of file paths. For directory symlinks:

- Store single directory path (e.g., ".claude")
- Keep same nuon format
- Cache operations remain unchanged

### Parallel Processing

Current implementation uses `par-each` extensively. For directory symlinks:

- Can still use `par-each` across multiple projects
- Directory symlink creation is atomic (single `ln -sf`)
- Simpler than file-based logic

## Testing Scenarios

1. **Basic directory symlink**
   - Create directory symlink with `link_directory: true`
   - Verify single symlink created

2. **Mixed configuration**
   - Some projects with `link_directory: true`, others false
   - Verify both behaviors work correctly

3. **Conflict detection**
   - Overlapping paths should error early
   - Clear error messages

4. **Force flag behavior**
   - Existing directory at target
   - Existing file at target
   - Existing symlink at target

5. **Cache behavior**
   - Switch from file to directory mode
   - Switch from directory to file mode
   - Verify cleanup works

6. **Teardown**
   - Directory symlinks removed correctly (only symlink, not contents)
   - No orphaned directories

## Code Organization

Suggest minimal changes to existing structure:

1. Extend `config.nu` to support optional `link_directory` column
2. Add conflict validation function in `config.nu` or new validation module
3. Split `get-project-files-to-link` to handle directory case separately
4. Keep existing `link` command flow with conditional logic

## Risk Assessment

- **Low Risk**: Opt-in feature, backward compatible
- **Medium Risk**: Conflict detection must be bulletproof
- **Mitigation**: Thorough validation before any filesystem changes

## Performance Notes

- Directory symlinks eliminate recursive traversal
- Significant speedup for large directories
- Cache updates are simpler (single entry vs many files)

## Edge Cases to Handle

1. **Symlink chains**: Source is already a symlink
2. **Permissions**: Target directory not writable
3. **Race conditions**: Directory modified during operation
4. **Special files**: Sockets, devices in source directory

## Future Enhancements

1. **Selective directory symlinks**: Link directory but exclude certain files
2. **Bidirectional sync detection**: Warn if changes in target
3. **Dry-run mode**: Preview operations without changes
4. **Rollback capability**: Undo last link operation
