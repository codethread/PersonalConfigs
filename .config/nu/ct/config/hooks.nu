export def main [] {
  {
    pre_prompt: [{ null }] # run before the prompt is shown
    pre_execution: [{ null }] # run before the repl input is run
    env_change: {
      PWD: [
        ...(fe-stuff),
      ]
    }
    display_output: "if (term size).columns >= 100 { table -e } else { table }" # run to display the output of a pipeline
    command_not_found: { null } # return an error message when a command is not found
  }
}

const fes = [
  ~/work/deals-light-ui
  ~/work/fe-review
  ~/work/fe-native
]

def fe-stuff [] {
  [
    {|before, after| if (is-fe $before) { print $"(ansi yellow) FE environment unloaded(ansi reset)"}  },
    {
      condition: {|before, after| is-fe $after },
      code: 'alias "git push" = gitlab push'
    },
    {|before, after| if (is-fe $after) { print $"(ansi blue) FE environment loaded(ansi reset)"}  },
  ]
}

def is-fe [dir?: path] {
  if ($dir |is-empty) { return false }
  $fes | path expand | filter {|p| $dir | str starts-with $p } | is-not-empty
}
