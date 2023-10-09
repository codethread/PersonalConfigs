source "init.nu"

const workp = ("~/.work.nu" | path expand)
source (if ($workp | path exists) { $workp } else { "empty.nu" })

const privates = ("~/.privates.nu" | path expand)
source (if ($privates | path exists) { $privates } else { "empty.nu" })

const atuin = ("~/.local/share/atuin/init.nu" | path expand)
source (if ($atuin | path exists) { $atuin } else { "empty.nu" })

const carapace = ("~/.cache/carapace/init.nu" | path expand)
source (if ($carapace | path exists) { $carapace } else { "empty.nu" })
