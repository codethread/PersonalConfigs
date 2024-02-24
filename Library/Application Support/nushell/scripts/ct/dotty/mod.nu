use ct/core *
use cache.nu [load-cache, save-cache, delete-cache]
use config.nu 

export def link [
  --no-cache(-c)
  --force(-f)
] {
  config load
  | par-each { |proj| get-project-files-to-link $proj $no_cache  }
  | assert-no-conflicts $force
  | par-each {|proj| 
    # delete
    $proj.delete | each {|f| rm -f $f }

    # create
    $proj.files | get to | list-dirs-to-make | par-each {|dir| mkdir $dir }

    # link TODO: could probably remove the stdout if link exists
    $proj.files | par-each { ln -s $in.from $in.to }

    # update cache
    $proj.existing ++ ($proj.files | get file) 
    | sort
    | save-cache $proj.name

    $proj 
    | rename --column { files: created, delete: deleted }
    | select name created deleted
  }
  | filter {|r| ($r.created | is-not-empty) or ($r.deleted | is-not-empty) }
  | table --collapse --flatten
}

# TODO: noticed some old files lingering.
export def list-dead-links [] {
}

# assumes run from PWD
# expected to be called by other tools, hence the errors
export def test [...files] {
  let proj = config load | where from == $env.PWD
  if ($proj | is-empty) {
    error make -u { msg: "not a project" }
  }

  let proj = $proj | first

  let files = $files | each {|f| [$env.PWD $f] | path join } | path expand

  let non_files = $files | filter { $in | path exists | $in == false }

  if ($non_files | is-not-empty) {
    error make -u { msg: (err_format "not real files" $non_files) }
  }

  let files = $files | path relative-to $env.PWD

  let all_files = list-files $proj.from --excludes $proj.excludes 

  let invalid = $files | where { $in not-in $all_files }

  if ($invalid | is-not-empty) {
    error make -u { msg: (err_format "non project files" $invalid) }
  }
}

def err_format [ title, files ] {
  $"($title):\n($files | to text)"
}

export def teardown [] {
  config load
  | par-each {|proj| 
      cd $proj.to
      let files = load-cache $proj.name 

      $files | par-each {|f| rm -f $f }

      delete-empty-dirs $files

      delete-cache $proj.name
  }
}

def delete-empty-dirs [files] {
  $files 
  | list-dirs-to-make
  | par-each {|dir| ls $dir | is-empty | if $in { $dir } else null }
  | compact
  | each {|dir| rm $dir }
}


# takes a list of files and returns a list of directories that will need
# to be created for them
def list-dirs-to-make []: list -> list  {
  path parse 
  | get parent 
  | uniq 
  | sort
  | reduce --fold [""] {|it, acc| 
    if ($it | str starts-with ($acc | last)) {
      let final_pos = ($acc | length) - 1
      $acc | upsert $final_pos $it
    } else {
      $acc | append $it
    }
  }
}

def get-project-files-to-link [proj, no_cache] {
    # cd in order to get all the gitignores correct
    cd $proj.from
    let cache = load-cache $proj.name

    let files = list-files $proj.from --excludes $proj.excludes

    let $new_files = $files | match ($no_cache) {
      true => { $in },
      false => { where { $in not-in $cache } },
    }

    # TODO having a no_cache option messes with deleting old files
    let to_delete = $cache | where { $in not-in $files }
    let existing = (if $no_cache { [] } else { $cache | where { $in not-in $to_delete } })

    {
      name: $proj.name,
      files:
        ($new_files | each {|file|
          { 
            file: $file,
            from:  ($proj.from | path join $file),
            to: ($proj.to | path join $file)
          }
        })
      delete: ($to_delete | each {|f| $proj.to | path join $f }),
      existing: $existing,
    }
}

def list-files [path, --excludes = []] {
  glob **/* --no-dir --exclude ([**/target/** **/.git/**] ++ $excludes)
  | path relative-to $env.PWD
  | list-not-ignored
}

def list-not-ignored [] {
  let files = $in
  let ignored = $files | to text | git check-ignore --stdin | lines

  $files | where { $in not-in $ignored } | sort
}

def assert-no-conflicts [force:bool] {
  let proj = $in

  let files = $proj
  | get files 
  | each {|| get to } 
  | reduce {|it,acc| $it ++ $acc }

  $files | uniq --repeated | match ($in | is-empty) {
    false => { 
      let msg = $"multiple files attempted to write to ($in | str join ","). Process cancelled"
      error make -u { msg: $msg }
    }
    _ => { $files }
  }

  let existing_files = $files | par-each {|file| { file: $file, exists: (file-exists $file) } } | where exists == true | get file

  if (($existing_files | is-not-empty) and $force) {
    $existing_files | each {|f| rm $f }
  } else if ($existing_files | is-not-empty) {
    print "A number of files already exist:"
    $existing_files | to text | print
    let choice = input $"would you like to remove these? [(ansi default_bold)Y(ansi reset)/n]: "

    if ($choice != 'n') {
      $existing_files | each {|f| rm $f }
    } else {
      error make -u { msg: "you'll have to remove them manually or change some file to proceed, alternativly you can pass the --force flag to overwrite them" }
    }
  }

  $proj
}

def file-exists [file: path] {
  ($file | path exists) and (is-symlink $file | not $in)
}

def is-symlink [file:path] {
  ($file | path exists) and (do { ^test -L $file } | complete | get exit_code | into bool | not $in )
}
