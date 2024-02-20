# alternative to fzf scripts which seem to fallover a lot
export def main [] {
  let attatched = (tmux list-session
  | lines
  | find "(attached)"
  | parse "{name}:{rest}"
  | get name
  | first)

  let sessions = (tmux list-session 
  | lines 
  | parse "{name}:{rest}"
  | get name
  | where $it !~ $attatched)

  let target = (echo $sessions
  | str join "\n"
  | fzf-tmux -p --preview $"($env.HOME)/.tmux/plugins/tmux-fzf/scripts/.preview {}" --preview-window ":follow")

  if ($target | is-empty) { return }
  tmux switch-client -t $target
}
