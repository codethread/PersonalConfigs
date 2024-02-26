use ct/core *

const timestamps = [start_date end_date last_modified creation_date]

# For each event in a table, update all the timestamp fields to show as nushell durations
export def update-time-to-duration [] {
  par-each {|r| $r | merge (echo $r | map-to-durations $timestamps --zero-year=2001) } 
}

# For a given set of columns, update each from a sqlite zero time to a nushell duration
def map-to-durations [
  columns: list<string>
  --zero-year: string
  --daylight-savings
]: table -> table {
  let row = $in

  echo $columns 
  | filter { |col| $row | get $col | is-not-empty }
  | reduce --fold {} { |col, dates| 
    let dur = (echo $row 
      | get $col 
      | from zero-time $zero_year --offset (if $daylight_savings { -1 } else { 0 }))

    $dates | upsert $col $dur
  }
}
