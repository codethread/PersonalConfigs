const NU_LIB_DIRS = [("~/dev/projects/nudes" | path expand)]

# use ct/core/typeof.nu

const format_year = "%Y-%m-%d"
const format_year_hr_min = "%Y-%m-%d %H:%M"


export def today [] {
  { 
    start: (date now | format date $format_year | into datetime)
    end: (date now | format date $format_year | into datetime | $in + 1day | $in - 1sec)
  }
}

export def "into zero-time" [
  year = "2001" # year of start, e.g 0000 for gregorian
] {
  let date = $in

  let transform = { |x| 
    $x - ($"($year)-01-01" | into datetime) 
    | format duration sec 
    | str substring 0..-4 
    | into float
  }

  match ($date | typeof) {
    { type: "record" } => {
      $date | columns | reduce --fold {} { |col, val| 
        $val | upsert $col (echo $date | get $col | do $transform $in)
      }
    }
    "date" => { do $transform $date }
  }
}

# Convert from a "since zero" duration, e.g from MySQL
# there are probably better ways to do this!
export def "from zero-time" [
  year = "2001" # year of start, e.g 0000 for gregorian
  --offset = 0 # offset to adjust by in hours
] {
  ($"($year)-01-01" | into datetime) + (($"($in)sec" | into duration) - ($"($offset)hr" | into duration))
}

use std assert

#[test]
def test_into_zero [] {
  assert equal 0 ("2001-01-01" | into datetime | into zero-time 2001)
  assert equal 721747800.00 ("2023-11-15 13:30" | into datetime | into zero-time 2001)
  (assert equal 
    ({ start: 721747800.00 end: 721747800.00 })
    ({ start: ("2023-11-15 13:30" | into datetime) end: ("2023-11-15 13:30" | into datetime) } | into zero-time 2001))
}

#[test]
def test_from_zero [] {
  assert equal "2001-01-01" (0 | from zero-time 2001 | format date $format_year)
  assert equal "2023-11-15 13:30" (721747800.00 | from zero-time 2001 | format date $format_year_hr_min)
}

def foo [] {
  ("2023-11-15 13:30" | into datetime | into zero-time 2001)
}

def typeof [] {
    let data = $in
    let raw_type = $data | describe

    match ($raw_type | str replace --regex "<.*" "") {
        "list" => { {
            type: "list"
            items: ($raw_type | parse "list<{type}>" | get type.0)
        } },
        "record" => {
            type: "record"
            fields: ($data | columns | each {|field| {
                name: $field,
                type: ($data | get $field | typeof)
            } } | transpose -rid)
        },
        "table" => {
            type: "table"
            columns: ($data | columns | each {|col| {
                name: $col,
                type: ($data | get $col | describe | parse "list<{type}>" | get type.0)
            } } | transpose -rid)
        },
        _ => $raw_type
    }
}
