;; extends

;; pending tests
(function_call
  name: (identifier) @keyword.bang
    (#eq? @keyword.bang "pending")
    (#set! priority 1000))

;; test stuff
(field
  name: (identifier) @keyword.bang
  value: (true)
    (#any-of? @keyword.bang "skip" "only")
    (#set! priority 1000))
