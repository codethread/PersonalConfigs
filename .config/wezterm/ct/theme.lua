local M = {}

M.palette = {
	base = "#232136",
	overlay = "#393552",
	muted = "#6e6a86",
	text = "#e0def4",
	love = "#eb6f92",
	gold = "#f6c177",
	rose = "#ea9a97",
	pine = "#3e8fb0",
	foam = "#9ccfd8",
	iris = "#c4a7e7",
	highlight_high = "#56526e",
}

local active_tab = {
	bg_color = M.palette.base,
	fg_color = M.palette.rose,
}

local inactive_tab = {
	bg_color = M.palette.base,
	fg_color = M.palette.text,
}

M.colors = {
	foreground = M.palette.text,
	background = M.palette.base,
	cursor_bg = M.palette.highlight_high,
	cursor_border = M.palette.highlight_high,
	cursor_fg = M.palette.text,
	selection_bg = M.palette.overlay,
	selection_fg = M.palette.text,

	ansi = {
		M.palette.overlay,
		M.palette.love,
		M.palette.pine,
		M.palette.gold,
		M.palette.foam,
		M.palette.iris,
		M.palette.rose,
		M.palette.text,
	},

	brights = {
		"#817c9c", -- replacement for muted,
		M.palette.love,
		M.palette.pine,
		M.palette.gold,
		M.palette.foam,
		M.palette.iris,
		M.palette.rose,
		-- '#ebbcba', -- replacement for rose,
		M.palette.text,
	},

	tab_bar = {
		background = M.palette.base,
		active_tab = {
			bg_color = M.palette.base,
			fg_color = M.palette.iris,
			-- "Half", "Normal" or "Bold"
			intensity = "Bold", -- non fancy
		},

		inactive_tab = {
			bg_color = M.palette.base,
			fg_color = M.palette.text,
		},

		inactive_tab_hover = active_tab,

		-- The same options that were listed under the `active_tab` section above
		-- effectivly hides the plus
		new_tab = {
			bg_color = M.palette.base,
			fg_color = M.palette.base,
		},

		new_tab_hover = active_tab,

		inactive_tab_edge = M.palette.base, -- (Fancy tab bar only)
	},
}

return M
