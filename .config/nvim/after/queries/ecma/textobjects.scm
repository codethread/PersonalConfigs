; inherits: ecma

;; imports, just named for now, e.g import {foo, baz} from 'bar'
(named_imports
  "," @_start .
  (import_specifier) @import.inner
  (#make-range! "import.outer" @_start @import.inner))

(named_imports
  . (import_specifier) @import.inner
  . ","? @_end
  (#make-range! "import.outer" @import.inner @_end))

