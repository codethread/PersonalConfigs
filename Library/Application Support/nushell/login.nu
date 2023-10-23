alias open = ^open
alias p = ^p
alias als = scope aliases

const has_nudes = ("~/dev/projects/nudes" | path expand | path exists)

const atuin = ("~/.local/share/atuin/init.nu" | path expand)
source (if ($has_nudes and ($atuin | path exists)) { $atuin } else { "empty.nu" })

const carapace = ("~/.cache/carapace/init.nu" | path expand)
source (if ($has_nudes and ($carapace | path exists)) { $carapace } else { "empty.nu" })
