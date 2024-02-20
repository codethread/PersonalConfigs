use ct/core [clog nud is-not-empty]
use barman.nu
use constants.nu [types]

# Homebrew enhancements
# mainly around installing will automatically be saved to a chosen profile (menu to stretch the brew metaphor)
# each menu can contain any number of cocktails, and those in turn contain drinks.
# These can later cleaned with "brew sync" which will remove anything not listed
# Also can be used on a new system to quick bootstrap

# Remove all brew related packages not listed on the current menu
export def sync [
  --log
  --dry-run # output brewfile contents but don't save or run anything
] {
  if ($log == true) { $env.CT_LOG = true }

  barman open 
    | barman menu get
    | brewfile new
    | match $dry_run {
        true => { $in },
        _ => {
          $in | clog "saving" | brewfile save
          ^brew bundle
          # probably want to clean after
        }
    }
}

# Install a tap, saving it to a given cocktail
export def tap [
  name: string
  --log
] {
  if ($log == true) { $env.CT_LOG = true }

  let config = barman open
  let taps = $config | barman menu get --taps

  if ($name in $taps) {
    print "Already installed"
    return
  }

  let cocktail = $config | barman prompt $name

  ^brew tap $name

  $config 
  | barman cocktail add $cocktail $name "tap"
  | barman write
}

export def install [
  formula: string
  ...flags: string
  --log
  --cask
] {
  if ($log == true) { $env.CT_LOG = true }

  let type = if ($cask) { $types.cask } else { $types.brew }
  clog "type" $type

  barman open
  | barman find $formula
  | clog "found" --expand
  | match ($in) {
      []                                        => { install_package $formula $type $flags }
      $package if (equal_flags $package $flags) => { reinstall_package $formula $type $flags }
      $package                                  => { store_package $package $flags }
    }
}

# Load the current BREWFILE into a nuon format
export def "bundle load" [--log] {
  if ($log == true) { $env.CT_LOG = true }

  let drinks = (nuopen ($env.HOMEBREW_BUNDLE_FILE | path expand) 
    | lines 
    | where ($it starts-with "#" | not $in) 
    | parse '{type} "{name}"'
    | each { |l| barman drink new $l.name $l.type }
    | clog "drinks")

  barman open
  | barman drinks add $drinks
  | barman write
}

# XXX needs work on the reinstall bit
def reinstall_package [
  formula: string 
  type: string
  flags: list<string>
] {
  let choice = input $"Formula ($formula) already installed, ignore or reinstall? [I, r]:"
  match $choice {
    "r" => { actually_install $formula $type $flags },
    _ => { print "ignoring" },
  }
}

# TODO not done, being lazy right now
def store_package [
  formula: record 
  flags: list<string>
] {
  print "already stored ->" ($formula | table --collapse) 
  print "you tried flags: " ($flags | table --collapse)
  print $"update file: (barman brew_packages_path)"
}

def install_package [
  formula: string 
  type: string
  flags: list<string>
] {

  let config = barman open
  let cocktail = $config | barman prompt $formula

  if (($cocktail | is-empty)) {
    print "nothing selected"
    return
  }

  actually_install $formula $type $flags

  let new_package = barman drink new $formula $type $flags

  $config
  | barman cocktail add $cocktail $formula $type
  | clog "output" --expand
  | barman write
}

def actually_install [
  formula: string 
  type: string
  flags: list<string>
] {

  let flags = (if ($type =~ "cask") { "--cask" ++ $flags } else $flags )
  { 
    formula: $formula
    flags: $flags
  } | clog "installing" --expand

  match $flags {
    []                  => { ^brew install $formula }
    [$a]                => { ^brew install $formula $a }
    [$a $b]             => { ^brew install $formula $a $b}
    [$a $b $c]          => { ^brew install $formula $a $b $c}
    [$a $b $c $d]       => { ^brew install $formula $a $b $c $d}
    [$a $b $c $d $e]    => { ^brew install $formula $a $b $c $d $e}
    [$a $b $c $d $e $f] => { ^brew install $formula $a $b $c $d $e $f}
    _                   => { print "too many flags! Update script"; exit 1 }
  }
}

def equal_flags [a b] {
  # I supsect this | first is a bug with how the nested list is stored
  (($a.flags | first | clean) == ($b | clean))
}

def clean [] {
  sort | uniq
}

def bundle [] {
  ^brew bundle dump --force --describe
}

# Create a brewfile string from a menu
# XXX might need to be sorted with taps first
def "brewfile new" [] {
  clog "creating brewfile"
  | par-each { |d| $"($d.type) \"($d.name)\"" }
  | str join "\n"
  | clog "brewfile:"
}

def "brewfile save" [] {
  clog $"saving brewfile ($env.HOMEBREW_BUNDLE_FILE)"
  | save --force ($env.HOMEBREW_BUNDLE_FILE | path expand)
}
