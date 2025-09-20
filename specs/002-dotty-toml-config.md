# Dotty TOML Configuration Migration

## Context and Problem Statement

The dotty symlink management tool currently uses hardcoded configuration embedded directly in the Nushell source code (`config/nushell/scripts/ct/dotty/config.nu`). This approach limits flexibility, requires code changes for configuration updates, and doesn't follow standard configuration management patterns. Users cannot customize their dotty configuration without modifying source code.

## Value Statement

Migrating to an external TOML configuration file will:

- Enable runtime configuration changes without code modification
- Follow XDG Base Directory standards for configuration management
- Improve maintainability by separating configuration from logic
- Allow users to version control their personal dotty configuration separately
- Provide a foundation for future configuration features (profiles, inheritance, etc.)

## Stakeholders

- **Users**: Developers using dotty for dotfile management
- **Maintainers**: Contributors to the dotty codebase
- **Dependencies**: Neovim integration, boot scripts, CI/CD systems using dotty

## Technical Architecture

The system will use a layered configuration approach:

1. **Configuration Loading**: Read TOML from XDG-compliant paths
2. **Fallback Mechanism**: Use default configuration when file is missing
3. **Validation Layer**: Ensure configuration meets schema requirements
4. **Backward Compatibility**: Maintain existing command-line interface

Configuration file location: `$XDG_CONFIG_HOME/dotty/dotty.toml` (defaults to `~/.config/dotty/dotty.toml`)

## Functional Requirements

### FR-1: TOML Configuration Loading

The system SHALL load configuration from a TOML file located at `~/.config/dotty/dotty.toml`, with proper support for XDG_CONFIG_HOME environment variable.

### FR-2: Configuration Schema

The configuration file SHALL support the following structure:

```toml
[global]
excludes = ["**/_?*/**", "**/.gitignore", "**/README.md"]

[[project]]
name = "string"           # Project identifier
real = "path"            # Source directory path (supports ~ expansion)
symlink = "path"         # Target symlink directory (supports ~ expansion)
excludes = ["patterns"]  # Additional exclusion patterns (optional)
link_directory = bool    # Whether to symlink entire directory (default: false)
```

### FR-3: Initial Configuration Setup

The initial TOML configuration file SHALL be created manually by:

- Creating the configuration directory at `~/.config/dotty/`
- Extracting the current hardcoded configuration from `config.nu`
- Writing a TOML file with all existing project definitions and settings
- Including global excludes from the hardcoded configuration

### FR-4: Path Expansion

The system SHALL expand tilde (`~`) and environment variables in path fields during configuration loading.

### FR-5: Configuration Validation

The system SHALL validate the configuration file and provide clear error messages for:

- Missing required fields
- Invalid path specifications
- Duplicate project names
- Invalid TOML syntax

### FR-6: Backward Compatibility

All existing public APIs SHALL remain unchanged:

- `dotty link` - Create symlinks
- `dotty prune` - Remove broken symlinks
- `dotty test` - Test if files are part of dotty
- `dotty teardown` - Remove all symlinks
- `dotty chmod` - Set permissions
- `dotty format` - Format output
- `dotty is-cwd` - Check if in dotty project

## Non-Functional Requirements

### NFR-1: Performance

Configuration loading SHALL complete in < 100ms for typical configurations (< 20 projects).

### NFR-2: Error Handling

The system SHALL:

- Fail with clear error messages for missing configuration file
- Fail with descriptive errors for malformed TOML
- Report specific issues for invalid paths
- Fail with descriptive message for file permission errors
- NOT fall back to defaults - configuration is user-defined

### NFR-3: Compatibility

The system SHALL work with:

- Nushell version 0.91.0 or later
- POSIX-compliant file systems
- macOS and Linux operating systems

### NFR-4: Maintainability

Configuration changes SHALL NOT require:

- Code compilation
- System restarts
- Cache clearing (beyond normal dotty cache behavior)

## External Dependencies

- Nushell's built-in `open` command for TOML parsing (validated via spike)
- XDG Base Directory specification
- File system operations (read, write, mkdir)

## Interface Definitions

### Configuration File Interface

```nu
# Load configuration with fallback to defaults
def load-config []: nothing -> table<name: string, real: path, symlink: path, excludes: list<path>, link_directory: bool>

# Migrate existing configuration to TOML
def migrate-config []: nothing -> nothing

# Validate configuration structure
def validate-config [config: table]: table -> table
```

## Acceptance Criteria

1. ✅ TOML configuration file can be created at `~/.config/dotty/dotty.toml`
2. ✅ Existing hardcoded configuration is migrated automatically on first run
3. ✅ All current dotty commands continue to work without changes
4. ✅ Configuration changes take effect without code modification
5. ✅ Clear error messages are shown for configuration issues
6. ✅ Path expansion works for home directory (`~`) references
7. ✅ Neovim integration continues to work seamlessly
8. ✅ Boot scripts using dotty continue to function

## Implementation Notes

### Technical Considerations

- Nushell has native TOML support via the `open` command
- Path expansion can use Nushell's `path expand` command
- Configuration caching may be needed for performance in the Neovim integration
- Consider adding a `dotty config` command to display current configuration

### Migration Strategy

1. Add TOML loading capability to config.nu
2. Implement automatic migration on first run
3. Update documentation with configuration examples
4. Consider deprecation timeline for hardcoded configuration

### Risk Mitigation

- **Risk**: Users may have scripts depending on specific configuration
- **Mitigation**: Maintain backward compatibility, provide migration tool

- **Risk**: TOML parsing errors could break dotty completely
- **Mitigation**: Implement fallback to default configuration

## Technical Debt Tracking

Future improvements to consider:

- Configuration profiles (work vs personal)
- Configuration inheritance/includes
- Configuration validation command (`dotty config --validate`)
- Web UI for configuration management
- Hot-reload of configuration changes
