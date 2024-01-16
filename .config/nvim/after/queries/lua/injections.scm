;; extends

;; highlight custom queries in vim.treesitter.parse_query(lang, [[ query ]])
;; invalid group in here, need to fix
; (function_call
;     name: (identifier) @_id (#eq? @_id "parse_query")
;     arguments: (arguments
;                 (string)
;                 (string ("string_content") @injection.context
;                         (#set! injection.language "lua")))

; (function_call
;     name: (dot_index_expression 
;             field: (identifier) @_id (#eq? @_id "parse_query"))
  ; name: (identifier) @_id (#eq? @_id "parse_query")
;     arguments: (arguments
;                 (string)
;                 (string ("string_content") @injection.context
;                         (#set! injection.language "lua")))
