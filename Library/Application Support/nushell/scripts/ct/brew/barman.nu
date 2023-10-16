use ct/core [nuopen clog]
use ct/brew/constants.nu [profiles types]

const brew_packages_path = ("~/.local/data/brew.nuon" | path expand)

# Get brew packages
# @returns Table {
#   profile: work | home | core
#   name: name of package
#   type: brew | cask
#   description?: string
#   options?: string[]
# }
export def open []: nothing -> table {
  echo $brew_packages_path
  | clog "opening packages:"
  | nuopen $in 
  | clog "packages"
}

export def write [] {
  let packages = $in

  echo $packages 
  | clog "saving"
  | to nuon --indent 2 
  | save --force $brew_packages_path

  $packages
}

export def find [
  name: string
  type: string
] {
  where name == $name | where type == $type
}

export def prompt [
  name: string
] {
  print $"Install new package ($name) to profile: [(join_profile_names)]"
}

def join_profile_names [] {
  $profiles 
| columns 
| enumerate 
| each { |name| 
    if ($name.index == 0) { $name.item | str capitalize } else { $name.item } 
  } 
| str join ","
}
