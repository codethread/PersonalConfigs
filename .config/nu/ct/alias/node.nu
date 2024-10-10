use ct/git [gchanged]

export def npc [] {
  rm -rf ./node_modules 
    npm cache clear
    npm i
}

export alias npl = npm -g ls --depth=0

export alias pp = pnpm

# INVALID multiline
# export alias yy = eval \$(cat package.json | jq -S '.scripts' | sed '1d;$d' | fzf -i --header='[run:]' | sed -E \"s/\\\"(.*)\\\":.*/yarn run \\1/\" )

export def pjs [...deps: string] {
  let search = ($deps | str join "|" | $"\(($in)\)")

  fd package.json
    | lines
    | par-each {|| open $in }
    | where {|| "dependencies" in $in }
    | par-each {|pj|
        let found = ($pj | get dependencies | transpose name version | where name =~ $search)
        if ($found | is-not-empty) {
          $found | insert package $pj.name
        }
    }
    | reduce {|it, acc| $acc ++ $it }
    | move package --before name
}

export def prettier-changed [--commit branch: string] {
  gchanged --commit=$commit
  | lines 
  | par-each { node_modules/.bin/prettier --write $in }
}

export def rn-nuke [] {
    git clean -dfX
    yarn 
    cd ios 
    pod install 
    cd .. 
    yarn run-ios --reset-cache
}

# print out the list of yarn workspace names
# useful in v1 where the commands are a bit shit
export def yarn-workspaces [] {
  # we have to skip the `yarn` output at the begging and end before we can parse
  yarn workspaces info | lines | skip 1 | drop 1 | to text | from json | columns
}

##############################################
# work
##############################################

export def nvm [] {
  volta pin node@14 
    volta pin yarn@1.22.4
}

export def react-native-clean [] { 
  git pull 
    gclean 
    yarn 
    yarn install:app 
    yarn run-ios --simulator "iPhone SE (3rd generation)"
}

export def npm-nope [] {
  if ('~/.npmrc' | path exists) {
    mv ~/.npmrc ~/.npmrc_nope
  } else if ('~/.npmrc_nope' | path exists) {
    mv ~/.npmrc_nope ~/.npmrc
  }
}
