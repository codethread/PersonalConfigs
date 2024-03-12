alias p = ^p
alias als = scope aliases

const atuin = ("~/.local/share/atuin/init.nu" | path expand)
source (if ($atuin | path exists) { $atuin } else { "empty.nu" })

const carapace = ("~/.cache/carapace/init.nu" | path expand)
source (if ($carapace | path exists) { $carapace } else { "empty.nu" })
