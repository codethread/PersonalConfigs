; extends

;; highlight custom queries in vim.treesitter.parse_query(lang, [[ query ]])
; (function_call
;     name: (dot_index_expression 
;             field: (identifier) @_id (#eq? @_id "parse_query"))
;     arguments: (arguments
;                 (string)
;                 (string ("string_content") @query)))
