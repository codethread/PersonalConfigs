vim.g.nord_italic = true
vim.g.nord_borders = true
vim.g.nord_contrast = true

-- local status_ok, nord = pcall(require, "nord")
-- if not status_ok then
-- 	vim.cmd([[
--     colorscheme slate
-- ]])

-- 	return
-- end

-- nord.set()
vim.cmd([[
  colorscheme nordbones

  " highlight Normal ctermbg=none guibg=none
  " highlight NonText ctermbg=none guibg=none
]])

local status_ok, lualine = pcall(require, "lualine")
if not status_ok then
	return
end

local mode_maps = {
	["NORMAL"] = "",
	["INSERT"] = "",
	["VISUAL"] = "",
	["V-BLOCK"] = "",
	["V-LINE"] = "",
	["TERMINAL"] = "❯",
	-- ["TERMINAL"] = "",
}

local function mode_map(str)
	if mode_maps[str] == nil then
		return str
	end
	return mode_maps[str]
end

lualine.setup({
	options = {
		icons_enabled = true,
		-- theme = "nord",
		theme = "nordbones",
		disabled_filetypes = {},
		section_separators = { left = "", right = "" },
		-- component_separators = { left = "", right = "" },
		component_separators = { left = "", right = "" },
	},
	sections = {
		lualine_a = {
			{
				"mode",
				separator = { left = " ", right = "" },
				right_padding = 2,
				fmt = mode_map,
			},
		},
		lualine_b = {
			{
				"filename",
				path = 1, -- relative path
				shorting_target = 40, -- leave at least 40 characters in line
				symbols = {
					modified = "  ",
					readonly = " ",
				},
			},
		},
		lualine_c = { "diagnostics", "lsp_progress" },
		lualine_x = {},
		lualine_y = { "filetype" },
		lualine_z = {
			{ "location", separator = { right = " " }, left_padding = 2 },
		},
	},
	inactive_sections = {
		lualine_a = {},
		lualine_b = {},
		lualine_c = {
			{
				"filename",
				path = 1, -- relative path
				shorting_target = 40, -- leave at least 40 characters in line
				-- let g:lightline.component = { 'readonlyS': '%{&readonly?"":""}', 'modifiedS': '%{&modified?" ":""}', }
				separator = { left = " " },
				right_padding = 2,
				symbols = {
					modified = "  ",
					readonly = " ",
				},
			},
		},
		lualine_x = {
			{ "location", separator = { right = " " }, left_padding = 2 },
		},
		lualine_y = {},
		lualine_z = {},
	},
	tabline = {
		lualine_a = {
			{
				"tabs",
				max_length = vim.o.columns / 2,
				mode = 2, -- tab name and number
				separator = { left = " ", right = " " },
				right_padding = 2,
			},
		},
		lualine_b = {},
		lualine_c = {},
		lualine_x = {},
		lualine_y = { "diff" },
		lualine_z = {
			{

				"branch",
				separator = { right = " ", left = "" },
				left_padding = 2,
			},
		},
	},
	extensions = {
		"quickfix",
	},
})
