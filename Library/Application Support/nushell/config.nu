use ct/boot
use ct/themes.nu themes
use ct/config/prompt.nu
use ct/config [keybindings menus hooks]
use ct/core *
# use ct/ical

alias open = open
alias open = ^open

use ct/tmux
use ct/git *
use ct/brew
use ct/dotty
use ct/purge.nu

$env.config = {
  show_banner: false,
  keybindings: [], # needed for atuin to not blow up
  hooks: {},
  completions: {}
  menus: []
}

let config = {
    show_banner: false # true or false to enable or disable the welcome banner at startup

    ls: {
        use_ls_colors: true # use the LS_COLORS environment variable to colorize output
        clickable_links: true # enable or disable clickable links. Your terminal has to support links.
    }

    rm: {
        always_trash: false # always act as if -t was given. Can be overridden with -p
    }

    table: {
        mode: rounded # basic, compact, compact_double, light, thin, with_love, rounded, reinforced, heavy, none, other
        index_mode: always # "always" show indexes, "never" show indexes, "auto" = show indexes when a table has "index" column
        show_empty: true # show 'empty list' and 'empty record' placeholders for command output
        padding: { left: 1, right: 1 } # a left right padding of each column in a table
        trim: {
            methodology: wrapping # wrapping or truncating
            wrapping_try_keep_words: true # A strategy used by the 'wrapping' methodology
            truncating_suffix: "..." # A suffix used by the 'truncating' methodology
        }
        header_on_separator: false # show header text on separator/border line
    }

    # datetime_format determines what a datetime rendered in the shell would look like.
    # Behavior without this configuration point will be to "humanize" the datetime display,
    # showing something like "a day ago."
    datetime_format: {
        # normal: '%a, %d %b %Y %H:%M:%S %z'    # shows up in displays of variables or other datetime's outside of tables
        # table: '%m/%d/%y %I:%M:%S%p'          # generally shows up in tabular outputs such as ls. commenting this out will change it to the default human readable datetime format
    }

    error_style: "fancy"

    display_errors: {
        # NOTE: may need to check how this behaves for scripts and nush
        exit_code: false # this shows a nushell error, which is annoying in repl
        termination_signal: true
    }

    explore: {
        try: {
            border_color: {fg: "white"}
        },
        status_bar_background: {fg: "#1D1F21", bg: "#C4C9C6"},
        command_bar_text: {fg: "#C4C9C6"},
        highlight: {fg: "black", bg: "yellow"},
        status: {
            error: {fg: "white", bg: "red"},
            warn: {}
            info: {}
        },
        table: {
            split_line: {fg: "#404040"},
            selected_cell: {},
            selected_row: {},
            selected_column: {},
            show_cursor: true,
            line_head_top: true,
            line_head_bottom: true,
            line_shift: true,
            line_index: true,
        },
        config: {
            border_color: {fg: "white"}
            cursor_color: {fg: "black", bg: "light_yellow"}
        },
    }

    history: {
        max_size: 100_000 # Session has to be reloaded for this to take effect
        sync_on_enter: true # Enable to share history between multiple sessions, else you have to close the session to write history to file
        file_format: "sqlite" # "sqlite" or "plaintext"
        # only available with sqlite file_format. true enables history isolation, false disables it. 
        # true will allow the history to be isolated to the current session using up/down arrows.
        # false will allow the history to be shared across all sessions.
        # disabled as this is purely per session... would be better with a custom function
        isolation: false 
    }

    filesize: {
        metric: true # true => KB, MB, GB (ISO standard), false => KiB, MiB, GiB (Windows standard)
        format: "auto" # b, kb, kib, mb, mib, gb, gib, tb, tib, pb, pib, eb, eib, auto
    }

    cursor_shape: {
        emacs: blink_block # block, underscore, line, blink_block, blink_underscore, blink_line (line is the default)
        vi_insert: blink_line # block, underscore, line , blink_block, blink_underscore, blink_line (block is the default)
        vi_normal: blink_block # block, underscore, line, blink_block, blink_underscore, blink_line (underscore is the default)
    }

    color_config: $themes.dark
    float_precision: 2 # the precision for displaying floats in tables
    use_ansi_coloring: true
    # ansi_coloring: 'auto'
    bracketed_paste: true # enable bracketed paste, currently useless on windows
    edit_mode: emacs # emacs, vi
    render_right_prompt_on_last_line: false # true or false to enable or disable right prompt to be rendered on last line of the prompt.

    use_kitty_protocol: true # who knows
    highlight_resolved_externals: true
}

$env.config = ($env.config | merge $config)

$env.config = ($env.config | upsert keybindings ($env.config.keybindings ++ $keybindings))

$env.config = ($env.config | upsert menus ($env.config.menus ++ menus))

$env.config = ($env.config | upsert completions ($env.config.completions | merge {
    case_sensitive: false # set to true to enable case-sensitive completions
    quick: true    # set this to false to prevent auto-selecting completions when only one remains
    partial: true    # set this to false to prevent partial filling of the prompt
    algorithm: "fuzzy"
    # external: {
    #     enable: true # set to false to prevent nushell looking into $env.PATH to find more suggestions, `false` recommended for WSL users as this look up may be very slow
    #     max_results: 100 # setting it lower can improve completion performance at the cost of omitting some options
    #     completer: null # check 'carapace_completer' above as an example
    # }
}))

$env.config = ($env.config | upsert hooks ($env.config.hooks | merge (hooks)))

const workp = ("~/.work.nu" | path expand)
source (if ($workp | path exists) { $workp } else { "empty.nu" })

const privates = ("~/.privates.nu" | path expand)
source (if ($privates | path exists) { $privates } else { "empty.nu" })
