;; extends

; execa style $ bash
; not working
(call_expression
  function: ((identifier) @_id 
                          (#eq? @_id "$"))
  arguments: ((template_string) @injection.content
                                (#offset! @injection.content 0 1 0 -1)
                                (#set! injection.language "sh")))

(call_expression
  function: (identifier) @_name
  (#any-of? @_name "css" "keyframes" "createGlobalStyle")
  arguments: (template_string) @injection.content
  (#offset! @injection.content 0 1 0 -1)
  (#set! injection.include-children)
  (#set! injection.language "styled"))
