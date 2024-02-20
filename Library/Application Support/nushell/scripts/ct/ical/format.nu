export def tmux [] {
  let upcoming = $in
  if $upcoming == null {
    return ""
  }
  let time = $upcoming.start_date | format date "%H:%M"

  let color = match ($upcoming.start_date - (date now)) {
    $x if $x < 1min => "#[fg=red]"
    $x if $x < 5min => "#[fg=yellow]"
    _               => "#[fg=cyan]"
  }

  $"($color)($time) ($upcoming.summary)#[fg=white]"
}
