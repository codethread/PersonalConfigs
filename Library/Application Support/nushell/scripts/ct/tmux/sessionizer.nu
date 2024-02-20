use ct/core *
use ct/tmux/utils.nu to-session
use ct/tmux/projects.nu [get-projects load-config]

export def main [
  x = -1
  --log
] {
  if ($log == true) { $env.CT_LOG = true }

  { project_id: $x } | clog "args"

  let project = (load-config
    | match $x {
      -1 => { $in | user-get-project },
      0..9 => { $in | get-projects | where key == $"P($x)" | get 0?.name },
      _    => { clog $"invalid selection, expected 0..9" }
    })

  if ($project | is-empty) {
    clog "no project selected";
    return
  } else {

    let $session = ($project | path basename | to-session)
    let $sessions = (get-sessions)

    clog "project:" $project
    clog "session:" $session
    clog "sessions:" $sessions

    # check if tmux is not running
    if (pgrep tmux | is-empty) {
      clog "new session:"
      tmux new-session -s $session -c ($project | path expand)
    }

    if ($session in $sessions) {
      clog "switching"
      tmux switch-client -t $session
    } else {
      clog "creating"
      tmux new-session -d -s $session -c ($project | path expand)
      tmux switch-client -t $session
    }
  }

}

############################################################################

def user-get-project [] {
  get dirs
  | path expand 
  | filter { || $in | path exists } 
  | each { |d| ls $d } 
  | flatten 
  | where type == 'dir' 
  | get name 
  | str join "\n" 
  | fzf-tmux -p
  | str trim
}

def get-sessions [] {
  tmux list-session 
  | lines 
  | parse "{name}:{rest}"
  | get name
}

