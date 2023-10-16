def home [p: string] {
  $nu.home-path | path join $p
}

def path-prepend [p: string] {
  let target = ($p | path expand)
  $env.PATH | prepend ($target)
}

$env.PATH = ($env.PATH | split row (char esep))

$env.CT_LOG = false

$env.CT_USER = (match (whoami) {
  "adam.hall" => "work", 
  "codethread" => "home",
  _ => "unknown", 
})

# HOMEBREW
$env.PATH = (path-prepend "/opt/homebrew/sbin")
$env.PATH = (path-prepend "/opt/homebrew/bin")

$env.HOMEBREW_BUNDLE_FILE = match $env.CT_USER {
  "home" => "~/.config/cold-brew/Brewfile.home.conf",
  "work" => "~/.config/cold-brew/Brewfile.work.conf",
  _ => "~/.config/cold-brew/Brewfile.basic.conf",
}

$env.HOMEBREW_CELLAR = "/opt/homebrew/Cellar"
$env.HOMEBREW_PREFIX = "/opt/homebrew"
$env.HOMEBREW_REPOSITORY = "/opt/homebrew"
# not sure if these matter?
# $env.INFOPATH = "/opt/homebrew/share/info:"
# $env.MANPATH = ([$env.MANPATH "/opt/homebrew/share/man:"] | str join)

$env.PATH = (path-prepend "~/.luarocks/bin")

$env.PATH = (path-prepend "~/.local/bin")

$env.PATH = (path-prepend "~/.cargo/bin")

$env.DOTFILES = (home PersonalConfigs)

$env.DOTTY = (match $env.CT_USER {
  "work" => ["~/PersonalConfigs" $nu.home-path "~/workfiles" $nu.home-path],
  _      => ["~/PersonalConfigs" $nu.home-path],
  } | path expand | str join ":")

$env.RUSTFLAGS = (match $env.CT_USER {
  "work" => "-C link-arg=-fuse-ld=/opt/homebrew/opt/llvm/bin/ld64.lld"
  _ => ""
})

$env.EDITOR = "nvim"
$env.SHELL = ("~/nu/nu" | path expand)
$env.MANPAGER = "/bin/sh -c \"col -b | nvim -c 'set ft=man ts=8 nomod nolist nonu' -\""
$env.MANWIDTH = 80
$env.LESSHISTFILE = "-" # no .lesshst

# EMACS
$env.LSP_USE_PLISTS = "true"
$env.PATH = (path-prepend "~/.emacs.d/bin")

# FZF
$env.FZF_ALT_C_COMMAND = "fd --hidden --type d --exclude '{Library,Music,Applications,Pictures,Unity,VirtualBox VMs,WebstormProjects,Tools,node_modules,.git}' . ~"
$env.FZF_CTRL_T_COMMAND = "fd --type f --hidden --exclude '{.git}'"
$env.FZF_DEFAULT_COMMAND = "fd --type f --hidden --exclude '{.git}'"

# GO
$env.PATH = (path-prepend "go/bin")
$env.GOBIN = (home "go/bin")
$env.GOPATH = (home "go")

# JAVASCRIPT / NODE
$env.PATH = (path-prepend ".volta/bin")
$env.VOLTA_HOME = (home ".volta")
$env.HUSKY = "0" # don"t hold my hand

# # ruby for gem install on m1 mac
# pathprepend "/opt/homebrew/opt/ruby/bin" PATH
# pathprepend "/opt/homebrew/lib/ruby/gems/3.1.0/bin" PATH

$env.WAKATIME_HOME = ($nu.home-path | path join ".config/wakatime")

$env.ZDOTDIR = ($nu.home-path | path join ".config/zsh")

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
    ($nu.default-config-dir | path join "scripts")
    ("~/dev/projects/nudes" | path expand)
    ("~/workfiles/nu" | path expand)
    ("~/dev/vendor/nu_scripts/sourced" | path expand)
]

# Directories to search for plugin binaries when calling register
$env.NU_PLUGIN_DIRS = [
   (home ".cargo/bin")
    # ($nu.default-config-dir | path join "plugins") # add <nushell-config-dir>/plugins
]

# keep this at the end
$env.PATH = (path-prepend "~/nu")
$env.PATH = ($env.PATH  | uniq)
