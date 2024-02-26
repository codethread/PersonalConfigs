use std assert

export def "assert eq-list" [a b] {
  assert (($a | clean) == ($b | clean)) "lists do not match"

}

def clean [] {
  sort | uniq
}
