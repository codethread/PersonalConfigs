contents = File.read("./tmux_template.conf")

# test_string = "hello {{PRIME_COLOR}}"

colours = {
        "{{PRIME_COLOR}}" => "colour81",
        "{{SECONDARY}}" => "colour153",
        "{{THIRD}}" => "colour242",
        "{{BASE}}" => "colour238",
        "{{BACKGROUND}}" => "colour235",
        "{{ERROR}}" => "colour197",
        "{{CONTRAST}}" => "colour185",
        "{{TEXT_CONTRAST}}" => "colour236",
        "{{WHITE}}" => "colour0",
}

result = contents.gsub(/{{(.+?)}}/, colours)

File.open("../../.tmux.conf", 'w') { |file| file.write(result) }

# when-changed -iv ./tmux_template.conf 'ruby ./color_config.rb'
