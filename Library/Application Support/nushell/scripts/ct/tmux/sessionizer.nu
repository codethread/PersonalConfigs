use ct/core *
use ct/tmux/utils.nu to-session

export def main [
  x?: int
  --log: bool
] {
  if ($log == true) { $env.CT_LOG = true }

  { project_id: $x } | clog "args"

  let project = if ($x != null) {
    clog $"getting project ($x)"
    get-projects 
    | where key == $"P($x)"
    | if ($in | is-empty) { null } else ($in | first | $in.name)
  } else {
    clog "getting projects from disk"
    user-get-project
  }

  if ($project | is-empty) {
    print "no project selected"
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
  let project_dirs = [
      ~/
      ~/dev ~/dev/exercism ~/dev/projects ~/dev/learn ~/dev/vendor
      ~/work ~/work/services ~/work/lambdas ~/work/utilities
      ~/.local/share/nvim/lazy ~/.local/share/nvim/mason/packages
  ] 

  $project_dirs 
    | path expand 
    | filter { || $in | path exists } 
    | each { |d| ls $d } 
    | flatten 
    | where type == 'dir' 
    | get name 
    | str join "\n" 
    | fzf-tmux
    | str trim
}

def get-sessions [] {
  tmux list-session 
    | lines 
    | parse "{name}:{rest}"
    | get name
}

def get-project-num [proj: string] {
  get-projects 
  | where key == $"P($proj)"
  | if ($in | is-empty) { null } else ($in | first | $in.name)
}

def get-projects [] {
  clog "reading projects from disk:"
  let data = ("~/.local/data/tmux.nuon" | path expand)
  clog $data
  let projects = (nuopen $data)
  clog "projects:" ($projects | table --expand)
  let base = [[key,name]; [P1, ($env.DOTFILES)]] | append $projects.base 

  let output = ($base ++ (if (is_work) { $projects.work } else { $projects.personal }))
  clog "output:" $output
  $output
}

