# Technical Notes: Dotty TOML Configuration Migration

## Spike Work Findings

### TOML Parsing in Nushell

✅ **Confirmed**: Nushell has built-in TOML support via the `open` command

- No external dependencies required
- Automatically parses TOML files into Nushell records/tables
- Tested with complex nested structures similar to dotty config

Example validation:

```nu
# Successfully parsed test configuration
open test.toml | describe
# Output: record<test: record<key: string, number: int>, array: table<name: string, value: int>>
```

### Path Expansion Behavior

Nushell's `path expand` command:

- Handles `~` expansion correctly
- Works with both `-n` flag (no checking) and without
- Example: `"~/PersonalConfigs" | path expand -n` → `/Users/codethread/PersonalConfigs`

### Current Configuration Structure

The existing hardcoded config in `config.nu`:

```nu
[
    [name, real, symlink, excludes, link_directory];
    [home, (dir ~/PersonalConfigs/home), (dir ~), [], false]
    [config, (dir ~/PersonalConfigs/config), (dir ~/.config), [], false]
    [claude, (dir ~/PersonalConfigs/claude), (dir ~/.claude), [], false]
    [claude-agents, (dir ~/PersonalConfigs/claude-agents), (dir ~/.claude/agents), [], true]
    [work, (dir ~/workfiles), (dir ~), [], false]
    [deals, (dir ~/workfiles/work/app/deals-light-ui/_git), (dir ~/work/app/deals-light-ui/.git), [], false]
]
```

## Implementation Considerations

### Configuration Loading Strategy

1. **Check for TOML file existence**

   ```nu
   let config_path = $"($env.XDG_CONFIG_HOME? | default $"($env.HOME)/.config")/dotty/dotty.toml"
   ```

2. **Load and parse TOML**

   ```nu
   let config = if ($config_path | path exists) {
       open $config_path
   } else {
       # Fall back to hardcoded config or create default
   }
   ```

3. **Path expansion and validation**
   ```nu
   $config.project
   | each { |proj|
       $proj
       | update real { |row| $row.real | path expand -n }
       | update symlink { |row| $row.symlink | path expand -n }
   }
   ```

### Migration Tool Design

The migration should:

1. Check if TOML config exists
2. If not, generate from hardcoded values
3. Create config directory if needed
4. Write TOML file with proper formatting

```nu
def migrate-config [] {
    let config_dir = $"($env.XDG_CONFIG_HOME? | default $"($env.HOME)/.config")/dotty"
    let config_path = $"($config_dir)/dotty.toml"

    if not ($config_path | path exists) {
        mkdir $config_dir
        # Generate TOML from existing config
        let toml_content = generate-toml-from-hardcoded
        $toml_content | save $config_path
    }
}
```

### Neovim Integration Impact

The Neovim integration calls:

- `dotty link | dotty format | to json -r`
- `dotty test <file>`
- `dotty is-cwd <path>`
- `dotty chmod`

All these commands will continue to work unchanged since we're only modifying the internal configuration loading mechanism.

### Performance Considerations

Current caching mechanism in `cache.nu`:

- Uses SHA-256 hashing of file metadata
- Stores in `~/.cache/dotty/cache.json`
- May need to add config file to cache invalidation triggers

### Error Handling Patterns

```nu
def load-config [] {
    try {
        let config_path = resolve-config-path
        open $config_path
        | validate-config
    } catch { |err|
        # Log error but don't fail
        print -e $"Warning: Failed to load config: ($err.msg)"
        # Return fallback config
        get-default-config
    }
}
```

## Breaking Changes Analysis

### No Breaking Changes to Public API

All exported functions maintain their signatures:

- ✅ `dotty link`
- ✅ `dotty prune`
- ✅ `dotty test`
- ✅ `dotty teardown`
- ✅ `dotty chmod`
- ✅ `dotty format`
- ✅ `dotty is-cwd`

### Internal Changes Only

- `config load` function will be modified to read TOML
- New internal functions for TOML parsing and validation
- Migration helper functions (can be removed after transition)

## Testing Strategy

1. **Unit Tests for Config Loading**
   - Test TOML parsing
   - Test path expansion
   - Test fallback behavior
   - Test validation

2. **Integration Tests**
   - Test all dotty commands with TOML config
   - Test migration process
   - Test Neovim integration

3. **Manual Testing Checklist**
   - [ ] Create symlinks with new config
   - [ ] Modify config and verify changes take effect
   - [ ] Test with missing config file
   - [ ] Test with malformed TOML
   - [ ] Test Neovim autocmds
   - [ ] Test boot scripts

## Security Considerations

- Config file permissions: Should be user-readable only (600 or 644)
- Path traversal: Validate that paths don't escape expected boundaries
- No execution of arbitrary code from config file

## Sample Migration Output

```toml
# Dotty configuration file
# Generated from hardcoded configuration on 2024-XX-XX

[global]
# Global exclusion patterns applied to all projects
excludes = [
    "**/_?*/**",       # Files/folders starting with underscore
    "**/.gitignore",   # Git ignore files
    "**/README.md"     # README files
]

[[project]]
name = "home"
real = "~/PersonalConfigs/home"
symlink = "~"
excludes = []
link_directory = false

[[project]]
name = "config"
real = "~/PersonalConfigs/config"
symlink = "~/.config"
excludes = []
link_directory = false

[[project]]
name = "claude"
real = "~/PersonalConfigs/claude"
symlink = "~/.claude"
excludes = []
link_directory = false

[[project]]
name = "claude-agents"
real = "~/PersonalConfigs/claude-agents"
symlink = "~/.claude/agents"
excludes = []
link_directory = true

[[project]]
name = "work"
real = "~/workfiles"
symlink = "~"
excludes = []
link_directory = false

# Git directory workaround
[[project]]
name = "deals"
real = "~/workfiles/work/app/deals-light-ui/_git"
symlink = "~/work/app/deals-light-ui/.git"
excludes = []
link_directory = false
```

## Open Questions Resolved

1. **Q: Should we support multiple config files?**
   A: Not in initial implementation. Keep it simple.

2. **Q: Should config be hot-reloaded?**
   A: No, require explicit `dotty link` execution.

3. **Q: How to handle config versioning?**
   A: Not needed initially, TOML structure is simple enough.

## Dependencies to Investigate

- No external dependencies required
- Nushell's built-in TOML support is sufficient
- XDG compliance is achieved through environment variable checking

## Rollback Plan

If issues arise:

1. Keep hardcoded config as fallback
2. Add feature flag to force hardcoded config
3. Document manual TOML editing as workaround

## Future Enhancements

- Config profiles (e.g., `dotty link --profile work`)
- Config includes (import other TOML files)
- Interactive config editor (`dotty config --edit`)
- Config validation command (`dotty config --check`)
- Support for conditional linking based on hostname/OS
