;; extends

;; highlight custom queries in vim.treesitter.parse_query(lang, [[ query ]])
;; still not working...
((function_call
     name: (identifier) @_id (#eq? @_id "parse_query")
     arguments: (arguments
                 (string)
                 (string (string_content) @injection.content)))
     (#set! injection.language "query"))


 ((function_call
     name: (dot_index_expression 
             field: (identifier) @_id (#eq? @_id "parse_query"))
     arguments: (arguments
                 (string)
                  (string (string_content) @injection.content)))
                         (#set! injection.language "query"))

((function_call
     name: (dot_index_expression
             table: (_)
             field: (identifier) @_id (#eq? @_id "nush"))
     arguments: (arguments 
                  (string
                    content: (_) @injection.content)))
     (#set! injection.language "nu"))
