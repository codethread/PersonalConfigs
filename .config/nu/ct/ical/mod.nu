use ct/core *
use time.nu [update-time-to-duration]
export use format.nu

const db = ("~/Library/Calendars/Calendar.sqlitedb" | path expand)

### NOTES
# invitation_status means not replied

# intended to be called by tmux status
export def coming-up [window = 10min] {
  let upcoming = next | update-time-to-duration
  let dur = $upcoming.start_date - (date now)
  if $dur < $window { $upcoming }
}

export def today [--hide-past]: nothing -> table {
  events adam | is-today --hide-past $hide_past
}

export def next [] {
  events adam 
  | is-today --hide-past true 
  | match $in {
      $x if ($x | is-empty) => { null },
      _ => { first }
    }
}

export def short-desc [length = 100]: table -> table {
  update description {|e| $e | default "" description | get description | str substring 0..$length }
}

export def short []: table -> table {
  select summary start_date end_date status has_attendees description
}

export def is-today [ --hide-past:bool = false ]: table -> table {
  let x = $in
  let t = (time today | into zero-time)
  let start = if $hide_past { date now | into zero-time } else { $t.start }
  $x | where start_date > $start and end_date < $t.end
}

# List out calendars
export def calendars []: nothing -> table {
  nuopen $db | get Calendar | select title ROWID
}

# List out all events for a given calendar
export def events [ calendar: string ]: nothing -> table {
  let calendar_id = (calendars | where title =~ $calendar | last | get ROWID)

# TODO: fix the if else stuff, echo doesn't work
  nuopen $db 
  | get CalendarItem 
  | where calendar_id == $calendar_id
  # | select summary location_id description start_date end_date all_day calendar_id status invitation_status availability url last_modified hidden has_recurrences has_attendees due_date creation_date app_link conference_url
}

