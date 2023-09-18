# Nushell Environment Config File
#
# version = "0.84.0"

$env.PATH = '/Users/codethread/.luarocks/bin:/Users/codethread/.cargo/bin:/Users/codethread/go/bin:/Users/codethread/.volta/bin:/Users/codethread/.local/bin:/opt/homebrew/sbin:/opt/homebrew/bin:/Applications/kitty.app/Contents/MacOS:/usr/bin:/bin:/usr/sbin:/sbin'

# $env.PATH = ($env.PATH | append "/Applications/Visual Studio Code.app/Contents/Resources/app/bin")

$env.DOTFILES = '/Users/codethread/PersonalConfigs'
$env.DOTTY = '/Users/codethread/PersonalConfigs:/Users/codethread'
$env.EDITOR = 'nvim'
$env.SHELL = (which nu | first | get path)

$env.FZF_ALT_C_COMMAND = "fd --hidden --type d --exclude '{Library,Music,Applications,Pictures,Unity,VirtualBox VMs,WebstormProjects,Tools,node_modules,.git}' . /Users/codethread"
$env.FZF_CTRL_T_COMMAND = "fd --type f --hidden --exclude '{.git}'"
$env.FZF_DEFAULT_COMMAND = "fd --type f --hidden --exclude '{.git}'"

$env.GOBIN = '/Users/codethread/go/bin'
$env.GOPATH = '/Users/codethread/go'

$env.HOMEBREW_BUNDLE_FILE = '/.config/cold-brew/Brewfile.mini.conf'
$env.HOMEBREW_CELLAR = '/opt/homebrew/Cellar'
$env.HOMEBREW_PREFIX = '/opt/homebrew'
$env.HOMEBREW_REPOSITORY = '/opt/homebrew'
$env.INFOPATH = '/opt/homebrew/share/info:'

$env.HUSKY = '0'

$env.LESSHISTFILE = '-'

$env.LSP_USE_PLISTS = 'true'

$env.MANPAGER = "/bin/sh -c \"col -b | nvim -c 'set ft=man ts=8 nomod nolist nonu' -\"" # XXX: treesitter issue "
# $env.MANPATH = '/opt/homebrew/share/man'

# $env.PATH = [/Users/codethread/.luarocks/bin, /Users/codethread/.cargo/bin, /Users/codethread/go/bin, /Users/codethread/.volta/bin, /Users/codethread/.local/bin, /opt/homebrew/sbin, /opt/homebrew/bin, /Applications/kitty.app/Contents/MacOS, /usr/bin, /bin, /usr/sbin, /sbin]

$env.VOLTA_HOME = '/Users/codethread/.volta'

$env.WAKATIME_HOME = '/Users/codethread/.config/wakatime'

$env.ZDOTDIR = '/Users/codethread/.config/zsh'

# Specifies how environment variables are:
# - converted from a string to a value on Nushell startup (from_string)
# - converted from a value back to a string when running external commands (to_string)
# Note: The conversions happen *after* config.nu is loaded
$env.ENV_CONVERSIONS = {
    "PATH": {
        from_string: { |s| $s | split row (char esep) | path expand --no-symlink }
        to_string: { |v| $v | path expand --no-symlink | str join (char esep) }
    }
    "Path": {
        from_string: { |s| $s | split row (char esep) | path expand --no-symlink }
        to_string: { |v| $v | path expand --no-symlink | str join (char esep) }
    }
}

# Directories to search for scripts when calling source or use
$env.NU_LIB_DIRS = [
    ($nu.default-config-dir | path join 'scripts') # add <nushell-config-dir>/scripts
    ($nu.home-path | path join dev/vendor/nu_scripts/sourced)
]

# Directories to search for plugin binaries when calling register
$env.NU_PLUGIN_DIRS = [
   ($nu.home-path | path join '.cargo/bin')
    # ($nu.default-config-dir | path join 'plugins') # add <nushell-config-dir>/plugins
]

# To add entries to PATH (on Windows you might use Path), you can use the following pattern:
# $env.PATH = ($env.PATH | split row (char esep) | prepend '/some/path')

# use ~/.config/nushell/scripts/prompt.nu my_theme

# my_theme

# module testy {
#     export def hello [name: string] {
#         $"hello ($name)!"
#     }
#
#     export def hi [where: string] {
#         $"hi ($where)!"
#     }
# }

# use testy *

