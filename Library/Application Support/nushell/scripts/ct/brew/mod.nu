use ct/core [clog nud is-not-empty]
use ct/brew/barman.nu
use ct/brew/constants.nu [types]

export def tap [
  name: string
] {
  let packages = (barman open)

  echo $packages 
    | upsert taps ($packages.taps ++ $name | uniq)
    | clog "taps"
    | barman write

  ^brew tap $name
}

export def install [
  formula: string
  ...flags: string
  --log: bool
  --cask: bool
] {
  if ($log == true) { $env.CT_LOG = true }

  let type = if ($cask) { $types.cask } else { $types.brew }
  clog "type" $type

  barman open
  | get packages 
  | barman find $formula $type 
  | clog "found"
  | match ($in) {
    [] => { install_package $formula $type $flags }
    $package => { store_package $package $flags }
  }
}

def install_package [
  formula: string 
  type: string
  flags: list<string>
] {
  print "instal->" $formula $type ($flags | str join)
}

def store_package [
  formula: record 
  flags: list<string>
] {
  print "store->" $formula $flags
}
