const nudes_repo = "https://github.com/codethread/nudes"
const install_dir = "~/dev/projects/nudes"

print $"(ansi yellow)No nudes detected(ansi reset)!
Want to install them from (ansi blue)($nudes_repo)(ansi reset) to (ansi cyan)($install_dir)(ansi reset)"

mut choice = "N"
try { $choice = (input $"[(ansi default_bold)N(ansi reset)/y]: ") }

if ($choice | str trim | str downcase) == "y" {
  print $"(ansi cyan)Installing(ansi reset)"

  git clone $nudes_repo ($install_dir | path expand)

  print $"(ansi cyan)Reloading(ansi reset)"

  exec nu
} else {
  print "Installation skipped"
}

