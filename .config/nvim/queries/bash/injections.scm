;; extends

((command
   name: (command_name) @_name (#eq? @_name "jq")
   argument: (raw_string) @injection.content)
 (#offset! @injection.content 0 1 0 -1)
 (#set! injection.language "jq"))

((command
   name: (command_name) @_name (#eq? @_name "awk")
   argument: (raw_string) @injection.content)
 (#offset! @injection.content 0 1 0 -1)
 (#set! injection.language "awk"))
