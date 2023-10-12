source "init.nu"

const workp = ("~/.work.nu" | path expand)
source (if ($workp | path exists) { $workp } else { "empty.nu" })

const privates = ("~/.privates.nu" | path expand)
source (if ($privates | path exists) { $privates } else { "empty.nu" })
