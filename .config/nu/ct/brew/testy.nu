use std assert

const NU_LIB_DIRS = [
    # directory of own helpers, all namespaced under dir "ct"
    # e.g ~/dev/projects/nudes/ct/core/is-not-empty.nu
    ("~/dev/projects/nudes" | path expand)
]

use ct/core [is-not-empty]

#[test]
def test_helpers [] {
    assert equal ([1 2] | is-not-empty) true
    assert equal ([] | is-not-empty) false
}
