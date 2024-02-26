# get a list of previously linked files
export def load-cache [name: string] {
  let conf = $"~/.local/data/dotty-cache-($name).nuon" | path expand
  match ($conf | path exists) {
    true => { nuopen $conf },
    false => { [] }
  }
}

export def save-cache [name: string] {
  let data = $in
  let conf = $"~/.local/data/dotty-cache-($name).nuon" | path expand
  $data | to nuon --indent 2 | save -f $conf
}

export def delete-cache [name: string] {
  let conf = $"~/.local/data/dotty-cache-($name).nuon" | path expand
  match ($conf | path exists) {
    true => { rm $conf },
  }
}
