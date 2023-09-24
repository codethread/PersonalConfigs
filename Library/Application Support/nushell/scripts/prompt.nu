# def create_left_prompt [] {
#     mut home = ""
#     try {
#         if $nu.os-info.name == "windows" {
#             $home = $env.USERPROFILE
#         } else {
#             $home = $env.HOME
#         }
#     }
#
#     let dir = ([
#         ($env.PWD | str substring 0..($home | str length) | str replace $home "~"),
#         ($env.PWD | str substring ($home | str length)..)
#     ] | str join)
#
#     let path_color = (if (is-admin) { ansi red_bold } else { ansi green_bold })
#     let separator_color = (if (is-admin) { ansi light_red_bold } else { ansi light_green_bold })
#     let path_segment = $"($path_color)($dir)"
#
#     $path_segment | str replace --all (char path_sep) $"($separator_color)/($path_color)"
# }

def create_right_prompt [] {
    # create a right prompt in magenta with green separators and am/pm underlined
    let time_segment = ([
        (ansi reset)
        (ansi magenta)
        (date now | format date '%Y/%m/%d %r')
    ] | str join | str replace --regex --all "([/:])" $"(ansi green)${1}(ansi magenta)" |
        str replace --regex --all "([AP]M)" $"(ansi magenta_underline)${1}")

    let last_exit_code = if ($env.LAST_EXIT_CODE != 0) {([
        (ansi rb)
        ($env.LAST_EXIT_CODE)
    ] | str join)
    } else { "" }

    ([$last_exit_code, (char space), $time_segment] | str join)
}


def create_left_prompt [] {
    starship prompt --cmd-duration $env.CMD_DURATION_MS $'--status=($env.LAST_EXIT_CODE)'
}

# Use nushell functions to define your right and left prompt

# The prompt indicators are environmental variables that represent
# the state of the prompt

# Use nushell functions to define your right and left prompt
# $env.PROMPT_COMMAND = {|| create_left_prompt }
# $env.PROMPT_COMMAND_RIGHT = {|| create_right_prompt }

# The prompt indicators are environmental variables that represent
# the state of the prompt
# $env.PROMPT_INDICATOR = {|| "> " }
# $env.PROMPT_INDICATOR_VI_INSERT = {|| ": " }
# $env.PROMPT_INDICATOR_VI_NORMAL = {|| "> " }
# $env.PROMPT_MULTILINE_INDICATOR = {|| "::: " }

export-env {
  $env.STARSHIP_SHELL = "nu"
  $env.PROMPT_COMMAND = { || create_left_prompt }
  $env.PROMPT_COMMAND_RIGHT = ""
  $env.PROMPT_INDICATOR = ""
  $env.PROMPT_INDICATOR_VI_INSERT = ": "
  $env.PROMPT_INDICATOR_VI_NORMAL = "〉"
  $env.PROMPT_MULTILINE_INDICATOR = "::: "
}
