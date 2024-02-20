export use time.nu *
# export use typeof.nu

export def nuopen [arg, --raw (-r)] { 
  if $raw { open -r $arg } else { open $arg } 
}

export def nup [arg] { prev $arg }

export def plugs [] {  
  help commands | where command_type == "plugin"
}

export def cmds [] {  
  help commands | where command_type == "custom" | reject params
}

export def nud [] { 
  let val = $in; 
  print $val; 
  $val 
}

export def pathis [] {
  $env.PATH
}

export def is-not-empty [] {
  is-empty | not $in
}

# logger: print arguments
export def clog [
  title = "log"
  --expand # expand piped input (assumes table input)
  ...args
] {
  let val = $in; 

  if ($env.CT_LOG) {
    print $"---- ($title) -----"

    $args | each { print $in }

    if $expand {
      print ($val | table --expand)
    } else { 
      print $val
    }
  }

  $val
}

export def is_work [] {
  (whoami) == 'adam.hall'
}

export def is_home [] {
  (whoami) == 'codethread'
}

