local M = {}

local palette = {
	base = '#232136',
	overlay = '#393552',
	muted = '#6e6a86',
	text = '#e0def4',
	love = '#eb6f92',
	gold = '#f6c177',
	rose = '#ea9a97',
	pine = '#3e8fb0',
	foam = '#9ccfd8',
	iris = '#c4a7e7',
	highlight_high = '#56526e',
}

local active_tab = {
	bg_color = palette.base,
	fg_color = palette.rose,
}

local inactive_tab = {
	bg_color = palette.base,
	fg_color = palette.text,
}

function M.colors()
	return {
		foreground = palette.text,
		background = palette.base,
		cursor_bg = palette.highlight_high,
		cursor_border = palette.highlight_high,
		cursor_fg = palette.text,
		selection_bg = palette.overlay,
		selection_fg = palette.text,

		ansi = {
			palette.overlay,
			palette.love,
			palette.pine,
			palette.gold,
			palette.foam,
			palette.iris,
			palette.rose,
			palette.text,
		},

		brights = {
			'#817c9c', -- replacement for muted,
			palette.love,
			palette.pine,
			palette.gold,
			palette.foam,
			palette.iris,
			palette.rose,
			-- '#ebbcba', -- replacement for rose,
			palette.text,
		},

		tab_bar = {
			background = palette.base,
			active_tab = {
				bg_color = palette.base,
				fg_color = palette.iris,
				-- "Half", "Normal" or "Bold"
				intensity = 'Bold', -- non fancy
			},

			inactive_tab = {
				bg_color = palette.base,
				fg_color = palette.text,
			},

			inactive_tab_hover = active_tab,

			-- The same options that were listed under the `active_tab` section above
			-- effectivly hides the plus
			new_tab = {
				bg_color = palette.base,
				fg_color = palette.base,
			},

			new_tab_hover = active_tab,

			inactive_tab_edge = palette.base, -- (Fancy tab bar only)
		},
	}
end

return M
