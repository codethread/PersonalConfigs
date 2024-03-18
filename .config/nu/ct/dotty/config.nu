use ct/core clog

# load in dotty config, currently in code but could be a nuon file
export def load [] {
    let excludes = ["**/_*/**", "**/.gitignore", "**/README.md"]

    let config = [
      [name, from, to, excludes];
      [dots, (dir ~/PersonalConfigs), (dir ~), 
        [
          "**/.luacheckrc"
          "**/.stylua.toml"
        ]
      ]
      [work, (dir ~/workfiles), (dir ~), ["**/README.md"]]
    ]

    $config 
    | upsert excludes { |project| $project.excludes ++ $excludes }
    | filter {|proj| $proj.from | path exists }
    | clog --expand

}

def dir [str: path] {
  $str | path expand
}
