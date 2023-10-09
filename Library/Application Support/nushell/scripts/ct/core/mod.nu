export alias als = scope aliases

export def nuopen [arg, --raw (-r)] { if $raw { open -r $arg } else { open $arg } }

export alias open = ^open

export def nup [arg] { prev $arg }

export alias p = ^p

export def plugs [] {  
  help commands | where command_type == "plugin"
}

export def cmds [] {  
  help commands | where command_type == "custom" | reject params
}

export def nud [] { let val = $in; print $val; $val }

export def pathis [] {
  $env.PATH
}

# logger: print arguments
export def clog [...args] {
  try {
    if ($env.CT_LOG) {
      $args | each { print $in }
    }
  }
}
