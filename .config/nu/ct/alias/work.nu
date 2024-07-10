use git.nu [gmm]

export def rg-phrase [
  text: string,
  --app # defaults to web
] {
  let app = if ($app) { "app" } else "web";
  let glob = $"apps/web/app/public/locale/($app)/en-gb/**/*"
  rg -g $glob --no-ignore $text
}

export def release [] {
  msg starting release

  msg Update master
  git checkout master
  git pull

  msg Update develop
  git checkout develop
  git pull

  git checkout master

  let log = git log --oneline --no-merges origin/master..origin/develop --pretty=format:"[%h] %<(15)(%an):  %s"

  git merge develop
  # git push

  let slackMsg = $":rocket: releasing develop\n```\n($log)\n```"
  print $slackMsg

  $slackMsg | pbcopy

  print ""
  print $"(ansi green)log copied to clipboard(ansi reset)"

  ^open https://git.perkbox.io/app/deals-light-ui/-/pipelines?page=1&scope=all&ref=master
  ^open https://app.slack.com/client/T02GD9LBZ/C1ZH7ME48
}

def msg [...msg: string] {
  print $"(ansi green)($msg | str join ' ')(ansi reset)"
}

# checkout an empty branch, handy for worktrees
export def nah [
  --update # update the ct-nah branches to latest develop
] {
  let name = $"ct-nah-($env.PWD | path basename)"

  if $update {
    git checkout $name
    gmm
    return
  }

  let exit_code = git rev-parse --verify $name | complete | get exit_code | into int

  if ($exit_code == 0) {
    git checkout $name
  } else {
    git checkout develop
    git checkout -b $name
  }
}
