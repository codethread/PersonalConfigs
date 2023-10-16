$env.config = {
  show_banner: false,
}

const has_nudes = ("~/dev/projects/nudes" | path expand | path exists)
source (if ($has_nudes) { "init.nu" } else { "boot/nudes.nu"})

const workp = ("~/.work.nu" | path expand)
source (if ($has_nudes and ($workp | path exists)) { $workp } else { "empty.nu" })

const privates = ("~/.privates.nu" | path expand)
source (if ($has_nudes and ($privates | path exists)) { $privates } else { "empty.nu" })
