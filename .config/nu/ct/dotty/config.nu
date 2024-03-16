# load in dotty config, currently in code but could be a nuon file
export def load [] {
    let excludes = ["**/_*/**"]

    let config = [
      [name, from, to, excludes];
      [dots, (dir ~/PersonalConfigs), (dir ~), 
        [
          "**/README.md",
          "**/.luacheckrc"
          "**/.stylua.toml"
          "**/.gitignore"
        ]
      ]
      [work, (dir ~/workfiles), (dir ~), ["**/README.md"]]
    ]

    $config 
    | upsert excludes { |project| $project.excludes ++ $excludes }
    | filter {|proj| $proj.from | path exists }
}

def dir [str: path] {
  $str | path expand
}