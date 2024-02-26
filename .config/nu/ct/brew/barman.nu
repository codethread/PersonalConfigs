use ct/core [nuopen clog is-not-empty];
use constants.nu [menus types];

# NOTES:
# `default` filter could be useful
# rethink flags - maybe just always empty array or something

# Barman manages the menu
# a menu contains cocktails
# each cocktail contains various drinks
# one drink is a brew formula/tap/cask

# XXX needs to handle missing env var
export def brew_packages_path [] {
 $env.DOTFILES | path join ".local/data/barman.nuon"
}

# Get the menu from disk
export def open [] {
  brew_packages_path
  | clog "opening packages:"
  | nuopen $in 
  | clog "packages" --expand
}

# Get the current menu based on CT_USER or validate name
def "menu name" [menu?: string]: table -> string {
  let config = $in
  let span = (metadata $menu).span
  let menu = (if ($menu == null) { $env.CT_USER } else $menu)

  $config | err-if-invalid-menu $menu
  $menu
}

# Get a menu if provided or infer from $env.CT_USER
# optional flags will only include selected flags
export def "menu get" [
  menu?: string
  --taps
  --casks
  --brews
] {
  let config = $in
  let span = (metadata $menu).span
  let menu = (if ($menu == null) { $env.CT_USER } else $menu)

  let drinks = (echo $config
    | err-if-invalid-menu $menu
    | get menus
    | get $menu 
    | clog "cocktails"
    | each { |name| $config.cocktails | get $name } 
    | flatten 
    | clog "drinks")
  
  let drinks = $config.drinks | where $it.name in $drinks

  if ($taps or $casks or $brews) {
    mut out = [[name type flags];[xxx xxx null]]
    if ($taps) { $out = $out ++ ($drinks | where $it.type == "tap") }
    if ($casks) { $out = $out ++ ($drinks | where $it.type == "cask") }
    if ($brews) { $out = $out ++ ($drinks | where $it.type == "brew") }
    $out | where $it.type != "xxx" # I have no idea how to not do this
  } else {
    $drinks
  }
}

# Update a given cocktail with a new drink
# XXX nothing to stop duplicates
export def "cocktail add" [cocktail: string, name: string, type: string]: table -> table {
  upsert cocktails { |config| 
    $config.cocktails | upsert $cocktail { |c| $c | get $cocktail | append $name }
  }
  | upsert drinks { |cs| $cs.drinks ++ { name: $name, type: $type } | uniq }
}

export def "drinks add" [drinks: table]: table -> table {
  upsert drinks { |config|
    ($config.drinks ++ $drinks) | uniq-by name | sort-by name
  }
  | clog "updated"
}

# pretty print, probably better ways but the default breaks over too many lines for easy edits
export def write [] {
  let packages = $in

  $packages | clog "writing" --expand

  let recipe = (echo $packages.drinks 
    | each {|l|
        let flags = (if ($l.options? | is-empty) { "null" } else $l.options )
        $"[ ($l.name), ($l.type), ($flags) ]"
      }
    | str join "\n    ")

  let output = $"{
  menus: ($packages.menus | pad-nuon)
  cocktails: ($packages.cocktails | pad-nuon)
  drinks: [
    [ name, type, flags ];
    ($recipe)
  ]
}"

  echo $output
  | clog "saving"
  | save --force (brew_packages_path)

  $packages
}

export def find [ name: string ] {
  get drinks
  | where name == $name
}

export def "drink new" [
  name: string 
  type: string
  flags?: list<string>
] {
  {
    name: $name
    type: $type
    flags: (if (($flags == null) or ($flags | is-empty)) { null } else $flags)
  }
}

export def prompt [ name: string ] {
  let config = $in
  let choice = (echo $config
    | get cocktails 
    | columns 
    | $in ++ "NEW"
    | input list $"Add new drink ($name) to cocktail:")

  match $choice {
    "NEW" => { create_new_cocktail $config },
    _ => { $choice }
  }
}

def create_new_cocktail [config: table] {
  let name = (input $"New name: " 
  | str trim 
  | str downcase
  | str replace --all ' ' '')

  if (($name | is-empty)) {
    print "nothing selected"
    return
  }

  let menu = $config | menu name

  echo $config 
  | upsert cocktails { |config| 
    $config.cocktails | upsert $name []
  }
  | upsert menus { |config|
    $config.menus | upsert $menu { |menus| 
      $menus | get $menu | append $name 
    }
  }
  | write

  print $"Created cocktail: ($name)"

  $name
}

def join_profile_names [] {
  get menus
  | columns 
  | enumerate 
  | each { |name| 
      if ($name.index == 0) { $name.item | str capitalize } else { $name.item } 
    } 
  | str join ", "
}

def err-if-invalid-menu [menu: string]: table -> table {
  let config = $in
  $config | clog "testing config"
  if ($menu not-in $config.menus) {
    let span = (metadata $menu).span
    error make {
      msg: $"($menu) is not on the menu!"
      label: {
        text: $"provided menu not one of ($config.menus | columns)"
        start: $span.start
        end: $span.end
      }
    }
  } else $config
}

def pad-nuon [] {
  to nuon --indent 2 | lines | each { |i| $"  ($i)" } | str join "\n"
}
