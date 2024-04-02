export def rg-phrase [
  text: string,
  --app # defaults to web
] {
  let app = if ($app) { "app" } else "web";
  let glob = $"apps/web/app/public/locale/($app)/en-gb/**/*"
  rg -g $glob --no-ignore $text
}

export def release [] {
  print $"(ansi green)starting release(ansi reset)"

  git checkout master
  git pull
  git fetch origin develop:develop

  let log = git log --oneline --no-merges origin/master..origin/develop --pretty=format:"[%h] %<(15)(%an):  %s"

  print $"(ansi green)log copied to clipboard(ansi reset)"

  git merge develop
  git push

  print $"(ansi green)log copied to clipboard(ansi reset)"

  print $log
  print ""
  let log = git log --oneline --no-merges origin/master..origin/develop --pretty=format:"[%h] %<(15)(%an):  %s"
  print $":rocket: releasing develop\n```\n($log)\n```"

  ^open https://git.perkbox.io/app/deals-light-ui/-/pipelines?page=1&scope=all&ref=master
  ^open https://app.slack.com/client/T02GD9LBZ/C1ZH7ME48
}
