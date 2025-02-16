--[[
--Reference and scratch for telescope pickers
--]]
--
local pickers = require 'telescope.pickers'
local finders = require 'telescope.finders'
local sorters = require 'telescope.sorters'
-- this is the user's conf
local conf = require('telescope.config').values

--   ╭─────────────────────────────────────────────────────────────────────────╮
--   │                             Shared options                              │
--   ╰─────────────────────────────────────────────────────────────────────────╯
local long_str = 'hey theoiaoi oiwejf sdofijsd flwe owiofijsdf oij oiwefsdf'
local opts_none = {}
local opts_wrapped = require('telescope.themes').get_cursor {
	wrap_results = true,
	layout_config = {
		cursor = {
			height = 10,
			preview_cutoff = 40,
			width = 0.2,
		},
	},
}

local opts = opts_wrapped

--   ╭─────────────────────────────────────────────────────────────────────────╮
--   │                             Simple pickers                              │
--   ╰─────────────────────────────────────────────────────────────────────────╯

-- literally the smallest possible telescope
-- usualy accessed via pickers.new which is a wrapper that also merges two tables with some minor validation
local min = pickers._Picker:new {
	finder = finders.new_table { 'red', 'green', 'blue' },
	sorter = sorters.get_generic_fuzzy_sorter(), -- uses default filter and highlights
}

local sorts = pickers.new({}, {
	finder = finders.new_table { 'red', 'green', 'blue' },
	-- sorter = sorters.empty(), -- literally nothing
	-- sorter = sorters.highlighter_only(), -- highlights but doesn't filter
	sorter = sorters.get_generic_fuzzy_sorter(), -- uses default filter and highlights
})

local min_with_entry = pickers.new({
	finder = finders.new_table {
		results = { 'red', 'green', 'blue', long_str },
		entry_maker = function(res) return { value = res, ordinal = res, display = res } end,
	},
	sorter = sorters.get_generic_fuzzy_sorter(),
}, opts)

--   ╭─────────────────────────────────────────────────────────────────────────╮
--   │                           entry maker formats                           │
--   ╰─────────────────────────────────────────────────────────────────────────╯

local displays = pickers.new({
	finder = finders.new_table {
		results = { 'red', 'green', 'blue', long_str },
		entry_maker = function(res)
			return {
				value = res,
				ordinal = res,
				display = function(entry)
					local highlights = {
						{ { 0, math.floor(#res / 2) }, '@comment.hint' },
					}
					return entry.value, highlights
				end,
			}
		end,
	},
	sorter = sorters.get_generic_fuzzy_sorter(),
}, opts)

--   ╭─────────────────────────────────────────────────────────────────────────╮
--   │                              with actions                               │
--   ╰─────────────────────────────────────────────────────────────────────────╯
local actions = require 'telescope.actions'
local action_state = require 'telescope.actions.state'
local with_action = pickers.new(opts_wrapped, {
	prompt_title = 'colors',
	finder = finders.new_table {
		results = { 'red', 'green', 'blue' },
		entry_maker = function(res) return { value = res, ordinal = res, display = res } end,
	},
	sorter = conf.generic_sorter {},
	attach_mappings = function(prompt_bufnr, map)
		actions.select_default:replace(function()
			actions.close(prompt_bufnr)
			dd(action_state.get_selected_entry())
		end)

		map('i', 'asdf', function(_prompt_bufnr) print 'You typed asdf' end)

		map({ 'i', 'n' }, '<C-r>', function(_prompt_bufnr)
			dd(action_state.get_selected_entry())
			dd(action_state.get_current_line())
			dd(action_state.get_current_history())
			print 'You typed <C-r>'
		end)

		return true -- this bit is important!!
	end,
})

--   ╭─────────────────────────────────────────────────────────────────────────╮
--   │                              other finders                              │
--   ╰─────────────────────────────────────────────────────────────────────────╯

local previewers = require 'telescope.previewers'
local make_entry = require 'telescope.make_entry'

local min_shot = pickers.new({}, {
	finder = finders.new_oneshot_job({ 'rg', '--vimgrep', 'vim' }, {
		entry_maker = make_entry.gen_from_vimgrep {},
	}),
	-- previewer = previewers.vim_buffer_vimgrep.new {},
	previewer = conf.grep_previewer(opts), -- defaults to above
	sorter = sorters.get_generic_fuzzy_sorter(),
})

local min_dynamic = pickers.new({}, {
	finder = finders.new_dynamic {
		entry_maker = function(res) return { value = res, ordinal = res, display = string.upper(res) } end,
		fn = function(prompt) return { 'hello' .. prompt, 'goodbye' .. prompt } end,
	},
	sorter = sorters.get_generic_fuzzy_sorter(),
})

local min_async = pickers.new({}, {
	finder = finders.new_async_job {
		entry_maker = function(res) return { value = res, ordinal = res, display = string.upper(res) } end,
		command_generator = function(prompt) return { 'ls', '-l', '-a', prompt } end,
	},
	sorter = sorters.get_generic_fuzzy_sorter(),
})
--   ╭─────────────────────────────────────────────────────────────────────────╮
--   │                                previews                                 │
--   ╰─────────────────────────────────────────────────────────────────────────╯

local custom_preview = pickers.new({
	dynamic_preview_title = true,
}, {
	finder = finders.new_oneshot_job({ 'fd', '--type', 'd', '--hidden', '--exclude', "'.git'" }, {
		entry_maker = make_entry.gen_from_file {},
	}),
	sorter = conf.generic_sorter(),
	previewer = previewers.new_termopen_previewer {
		-- title = 'hey there title!',
		dyn_title = function(self, entry) return 'hey' .. entry.value end,
		get_command = function(entry, status)
			return {
				'ls',
				'-l',
				'-a',
				entry.value,
			}
		end,
	},
})

--   ╭─────────────────────────────────────────────────────────────────────────╮
--   │                                 layouts                                 │
--   ╰─────────────────────────────────────────────────────────────────────────╯
local builtin = require 'telescope.builtin'
local themes = require 'telescope.themes'

local layout_play = function()
	builtin.find_files {
		layout_strategy = 'flex',
	}
end

layout_play()
-- local run = min_shot
-- run:find()
