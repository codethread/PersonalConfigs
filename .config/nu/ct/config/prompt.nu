def create_right_prompt [] {
    let time_segment = ([
        (ansi reset)
        (ansi magenta)
        (date now | format date '%Y/%m/%d %r')
    ] | str join 
      | str replace --regex --all "([/:])" $"(ansi green)${1}(ansi magenta)" 
      | str replace --regex --all "([AP]M)" $"(ansi magenta_underline)${1}")

    $time_segment
}


def create_left_prompt [] {
    starship prompt --cmd-duration $env.CMD_DURATION_MS $'--status=($env.LAST_EXIT_CODE)'
}

export-env {
  $env.STARSHIP_SHELL = "nu"
  $env.PROMPT_COMMAND = { || create_left_prompt }
  $env.PROMPT_COMMAND_RIGHT = { || starship prompt --right }
  $env.PROMPT_INDICATOR = ""
  $env.PROMPT_INDICATOR_VI_INSERT = ": "
  $env.PROMPT_INDICATOR_VI_NORMAL = "〉"
  $env.PROMPT_MULTILINE_INDICATOR = "::: "
}
