let conf = ($nu.config-path)
let ev = ($nu.env-path)

print $"removing ($conf)"
print $"removing ($ev)"

zsh -c $"rm ($conf) && rm $($ev)"

/opt/homebrew/bin/dotty setup
