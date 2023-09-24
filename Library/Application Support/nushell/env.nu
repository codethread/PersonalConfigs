def home [p: string] {
  $nu.home-path | path join $p
}

def path-prepend [p: string] {
  let target = ($p | path expand)
  $env.PATH | prepend ($target)
}

$env.PATH = ($env.PATH | split row (char esep))

$env.CT_IS_WORK = (whoami) == "adam.hall"
$env.CT_IS_HOME = (whoami) == "codethread"
$env.CT_USER = match (whoami) {
  "adam.hall" => "work", 
  "codethread" => "home",
  _ => "unknown", 
} # XXX: "

# HOMEBREW
$env.PATH = (path-prepend '/opt/homebrew/sbin')
$env.PATH = (path-prepend '/opt/homebrew/bin')

$env.HOMEBREW_BUNDLE_FILE = match $env.CT_USER {
  "home" => '~/.config/cold-brew/Brewfile.mini.conf',
  "work" => '~/.config/cold-brew/Brewfile.work.conf',
  _ => '~/.config/cold-brew/Brewfile.basic.conf',
} # XXX: "

$env.HOMEBREW_CELLAR = '/opt/homebrew/Cellar'
$env.HOMEBREW_PREFIX = '/opt/homebrew'
$env.HOMEBREW_REPOSITORY = '/opt/homebrew'
# not sure if these matter?
# $env.INFOPATH = '/opt/homebrew/share/info:'
# $env.MANPATH = ([$env.MANPATH '/opt/homebrew/share/man:'] | str join)

$env.PATH = (path-prepend '~/.luarocks/bin')

$env.PATH = (path-prepend '~/.local/bin')

$env.PATH = (path-prepend '~/.cargo/bin')

$env.DOTFILES = (home PersonalConfigs)

$env.DOTTY = (if $env.CT_IS_WORK {
    ["~/PersonalConfigs" $nu.home-path "~/workfiles" $nu.home-path] 
  } else {
    ["~/PersonalConfigs" $nu.home-path]
  }
    | path expand | str join ":")

if $env.CT_IS_WORK {
  $env.RUSTFLAGS = "-C link-arg=-fuse-ld=/opt/homebrew/opt/llvm/bin/ld64.lld"
}

$env.EDITOR = 'nvim'
$env.SHELL = (which nu | first | get path)
$env.MANPAGER = "/bin/sh -c \"col -b | nvim -c 'set ft=man ts=8 nomod nolist nonu' -\"" # XXX: treesitter issue "
$env.MANWIDTH = 80
$env.LESSHISTFILE = '-' # no .lesshst

# EMACS
$env.LSP_USE_PLISTS = 'true'
$env.PATH = (path-prepend '~/.emacs.d/bin')

# FZF
$env.FZF_ALT_C_COMMAND = "fd --hidden --type d --exclude '{Library,Music,Applications,Pictures,Unity,VirtualBox VMs,WebstormProjects,Tools,node_modules,.git}' . ~"
$env.FZF_CTRL_T_COMMAND = "fd --type f --hidden --exclude '{.git}'"
$env.FZF_DEFAULT_COMMAND = "fd --type f --hidden --exclude '{.git}'"

# GO
$env.PATH = (path-prepend 'go/bin')
$env.GOBIN = (home 'go/bin')
$env.GOPATH = (home 'go')

# JAVASCRIPT / NODE
$env.PATH = (path-prepend '.volta/bin')
$env.VOLTA_HOME = (home '.volta')
$env.HUSKY = '0' # don't hold my hand

# # ruby for gem install on m1 mac
# pathprepend "/opt/homebrew/opt/ruby/bin" PATH
# pathprepend "/opt/homebrew/lib/ruby/gems/3.1.0/bin" PATH

$env.WAKATIME_HOME = ($nu.home-path | path join '.config/wakatime')

$env.ZDOTDIR = ($nu.home-path | path join '.config/zsh')

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
    (home 'dev/vendor/nu_scripts/sourced')
]

# Directories to search for plugin binaries when calling register
$env.NU_PLUGIN_DIRS = [
   (home '.cargo/bin')
    # ($nu.default-config-dir | path join 'plugins') # add <nushell-config-dir>/plugins
]

# keep this at the end
$env.PATH = ($env.PATH  | uniq)

const empty = ($nu.default-config-dir | path join "empty.nu")
const privates = ("~/.privates.nu" | path expand)
const workp = ("~/.work.nu" | path expand)

source (if ($privates | path exists) { $privates } else { $empty })
source (if ($workp | path exists) { $workp } else { $empty })
