# To configure on iTerm, make a keybinding (presumably for C-r) whose action is "Send text with vim special chars",
#    and use: \u0001fzf-history --query="\u0005"\u000d
#    where \u0001 is for the start of a line, \u0005 is the end of a line, and \u000d is RET

# fzf through shell history, typing result.
def fzf-history [
	--query (-q): string # Optionally start with given query.
] {
    let cmd = (history | uniq | reverse | each { echo [$it (char nl)] } | str collect | fzf --query $"($query)")
    osascript -e $'tell application "System Events" to keystroke "($cmd)"'
}
