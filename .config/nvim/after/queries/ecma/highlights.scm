; inherits: ecma

; so much signficance for such a small char
"!" @keyword.bang

"default" @keyword.default
"export" @keyword.export
"throw" @keyword.export

; highlight arrow functions
(lexical_declaration
  kind: "const" @arrow_function.const
  (variable_declarator
	value: (arrow_function "=>" @arrow_function.arrow)))

; highlight arrow functions
; (export_statement "export" @keyword.export (set! "priority" 105))
; (export_statement "export" @keyword.export (set! "priority" 105) "default" @keyword.default)
