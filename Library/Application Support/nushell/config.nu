use ct/boot
use ct/themes.nu themes
use ct/config/prompt.nu
use ct/config [keybindings menus hooks]
use ct/core *
# use ct/ical

# alias open = open
# alias open = ^open

use ct/tmux
use ct/git *
use ct/brew
use ct/dotty
use ct/purge.nu

$env.config = {
    keybindings: [], # needed for atuin to not blow up
    hooks: {},
    menus: []

    completions: {
        algorithm: "fuzzy"
    }

    show_banner: false # true or false to enable or disable the welcome banner at startup

    table: {
        mode: light # default 'rounded', also 'markdown' handy for scripts
        index_mode: auto
        show_empty: false # show 'empty list' and 'empty record' placeholders for command output
    }

    display_errors: {
        # NOTE: may need to check how this behaves for scripts and nush
        exit_code: false # this shows a nushell error, which is annoying in repl
        termination_signal: true
    }

    history: {
        max_size: 100_000 # Session has to be reloaded for this to take effect
        sync_on_enter: true # Enable to share history between multiple sessions, else you have to close the session to write history to file
        file_format: "sqlite" # "sqlite" or "plaintext"
    }

    cursor_shape: {
        emacs: blink_block # block, underscore, line, blink_block, blink_underscore, blink_line (line is the default)
        vi_insert: blink_line # block, underscore, line , blink_block, blink_underscore, blink_line (block is the default)
        vi_normal: blink_block # block, underscore, line, blink_block, blink_underscore, blink_line (underscore is the default)
    }

    color_config: $themes.dark
    edit_mode: emacs # emacs, vi
    render_right_prompt_on_last_line: false # true or false to enable or disable right prompt to be rendered on last line of the prompt.
    use_kitty_protocol: true # who knows
    highlight_resolved_externals: true
}

$env.config = ($env.config | upsert keybindings ($env.config.keybindings ++ $keybindings))

$env.config = ($env.config | upsert menus ($env.config.menus ++ menus))

$env.config = ($env.config | upsert hooks ($env.config.hooks | merge (hooks)))

const workp = ("~/.work.nu" | path expand)
source (if ($workp | path exists) { $workp } else { "empty.nu" })

const privates = ("~/.privates.nu" | path expand)
source (if ($privates | path exists) { $privates } else { "empty.nu" })
