;; extends

((command
    name: (command_name) @_name (#eq? @_name "jq")
    argument: (raw_string) @jq)
    (#offset! @jq 0 1 0 -1))

((command
    name: (command_name) @_name (#eq? @_name "awk")
    argument: (raw_string) @awk)
    (#offset! @awk 0 1 0 -1))
