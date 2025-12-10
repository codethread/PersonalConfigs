export alias _kitty-man = man -k -M "/Applications/kitty.app/Contents/Resources/man" "."
# filter out noisy layout info from kitty @ ls
export alias _kitty-ls-jq = jq 'map({ tabs: .tabs |= map(del(.enabled_layouts, .groups, .layout_opts, .layout_state)) })'

# install nighlty
export def _kitty-upgrade [] { curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin installer=nightly }
export alias _kitty-diff = git difftool --no-symlinks --dir-diff
