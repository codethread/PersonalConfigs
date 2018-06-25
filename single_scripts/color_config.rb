contents = File.read("./tmux_template.conf")

# test_string = "hello {{PRIME_COLOR}}"

tender_colours = {
        "{{PRIME_COLOR}}" => "colour81",
        "{{SECONDARY}}" => "colour153",
        "{{BASE_EMPHASIS}}" => "colour242",
        "{{BASE}}" => "colour238",
        "{{BACKGROUND}}" => "colour235",
        "{{ERROR}}" => "colour197",
        "{{CONTRAST}}" => "colour185",
        "{{TEXT_CONTRAST}}" => "colour236",
        "{{BLACK}}" => "colour234",
        "{{LIGHT_GREY}}" => "colour250",
        "{{WHITE}}" => "colour255",
}

snazzy_colours = {
        "{{PRIME_COLOR}}" => "magenta",
        "{{TEXT}}" => "blue",
        "{{BASE}}" => "#686868",
        "{{BACKGROUND}}" => "#282a36",
}

result = contents.gsub(/{{(.+?)}}/, snazzy_colours)

File.open("../../.tmux.conf", 'w') { |file| file.write(result) }

# when-changed -iv ./tmux_template.conf 'ruby ./color_config.rb'
