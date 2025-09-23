# :module: TOML configuration loader and validator for dotty symlink projects

use ct/core clog

# load in dotty config from TOML file
# Checks XDG_CONFIG_HOME first, then falls back to ~/.config
export def load [
	config_path?: path # override config path for testing or bootstrapping a new system
]: nothing -> table<name: string, origin: path, target: path, excludes: list<path>, link_directory: bool> {
	let config_file = $config_path | default (get-config-path)

	# Check if the configuration file exists
	if not ($config_file | path exists) {
		error make {
			msg: $"Error: Configuration file not found at ($config_file)"
			help: "Create a TOML configuration file at ~/.config/dotty/dotty.toml"
		}
	}

	load-config-from-toml $config_file | clog --expand
}

def get-config-path [] {
	let config_dir = if ($env.XDG_CONFIG_HOME? | is-not-empty) {
		$env.XDG_CONFIG_HOME
	} else {
		"~/.config" | path expand
	}
	let config_file = $config_dir | path join "dotty" "dotty.toml"

	$config_file
}


# Validate TOML configuration schema and content
def validate-toml-config [toml_config: record] {
	# Check for duplicate project names
	if "project" in $toml_config {
		let project_names = $toml_config.project | get name
		let unique_names = $project_names | uniq

		if ($project_names | length) != ($unique_names | length) {
			let duplicates = $project_names | group-by { |x| $x } | items { |name, items| if ($items | length) > 1 { $name } else { null } } | compact
			error make {
				msg: $"Error: Duplicate project names found in configuration: (($duplicates | str join ', '))"
				help: "Each project must have a unique name"
			}
		}

		# Validate each project
		for proj in $toml_config.project {
			# Check required fields
			if not ("name" in $proj) {
				error make {
					msg: "Error: Missing required field 'name' in project configuration"
					help: "Each project must have a 'name' field"
				}
			}

			if not ("origin" in $proj) {
				error make {
					msg: $"Error: Missing required field 'origin' in project '($proj.name)'"
					help: "Each project must have an 'origin' path field"
				}
			}

			if not ("target" in $proj) {
				error make {
					msg: $"Error: Missing required field 'target' in project '($proj.name)'"
					help: "Each project must have a 'target' path field"
				}
			}

			# Validate data types
			if ("excludes" in $proj) and not (($proj.excludes | describe) | str starts-with "list") {
				error make {
					msg: $"Error: Field 'excludes' in project '($proj.name)' must be a list"
					help: "The excludes field should be an array of strings"
				}
			}

			if ("link_directory" in $proj) and not (($proj.link_directory | describe) == "bool") {
				error make {
					msg: $"Error: Field 'link_directory' in project '($proj.name)' must be a boolean"
					help: "Use true or false for the link_directory field"
				}
			}
		}
	}

	# Validate global section if present
	if "global" in $toml_config {
		if ("excludes" in $toml_config.global) and not (($toml_config.global.excludes | describe) | str starts-with "list") {
			error make {
				msg: "Error: Field 'excludes' in global section must be a list"
				help: "The global excludes field should be an array of strings"
			}
		}
	}

	$toml_config
}

# Load configuration from TOML file
def load-config-from-toml [config_file: path]: nothing -> table<name: string, origin: path, target: path, excludes: list<path>, link_directory: bool> {
	# Load and parse the TOML file with enhanced error handling
	try {
		let raw_toml = open $config_file
		let toml_config = validate-toml-config $raw_toml

		# Extract global excludes if they exist
		let global_excludes = if "global" in $toml_config and "excludes" in $toml_config.global {
			$toml_config.global.excludes
		} else {
			[]
		}

		# Process project configurations with optimized path expansion
		# Skip path existence validation during initial load for better performance
		let projects = if "project" in $toml_config {
			$toml_config.project | each { |proj|
				# Expand paths once (no validation yet)
				let origin_path = ($proj.origin | path expand)
				# expand for things like `~` but don't resolve symlinks or we end up in a cycle.
				# we want the target to be exactly as the config is saying
				let target_path = ($proj.target | path expand --no-symlink)

				{
					name: $proj.name,
					origin: $origin_path,
					target: $target_path,
					excludes: (if "link_directory" in $proj and $proj.link_directory {
						[]
					} else if "excludes" in $proj {
						$proj.excludes
					} else {
						[]
					}),
					link_directory: (if "link_directory" in $proj { $proj.link_directory } else { false })
				}
			}
		} else {
			[]
		}

		# Combine project-specific excludes with global excludes and apply lazy filtering
		$projects
		| each { |project|
			$project | upsert excludes { |proj|
				if $proj.link_directory {
					[]  # No excludes for directory symlinks
				} else {
					$proj.excludes ++ $global_excludes
				}
			}
		}
		| where {|proj| $proj.origin | path exists }  # Filter only once at the end

	} catch { |err|
		# Provide specific error messages based on the error type
		let error_msg = if ($err.msg | str contains "parse") {
			$"Error: Invalid TOML syntax in configuration file at ($config_file)"
		} else if ($err.msg | str contains "not found") {
			$"Error: Configuration file structure is invalid at ($config_file)"
		} else {
			$"Error: Failed to load configuration from ($config_file)"
		}

		error make {
			msg: $error_msg
			help: $"TOML error details: ($err.msg)\nEnsure the configuration file has valid TOML syntax and required fields"
		}
	}
}
