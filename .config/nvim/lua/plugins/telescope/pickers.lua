local pickers = require 'telescope.pickers'
local previewers = require 'telescope.previewers'
local finders = require 'telescope.finders'
local conf = require('telescope.config').values
local make_entry = require 'telescope.make_entry'

local function unsaved(opts)
	opts = opts or {}
	local results = {}
	-- local diff = vim.cmd [[w !git diff --no-index -- % -]]

	for _, buf in ipairs(vim.api.nvim_list_bufs()) do
		local is_modified = vim.api.nvim_buf_get_option(buf, 'modified')

		if is_modified then
			local file = vim.api.nvim_buf_get_name(buf)

			table.insert(results, file)
		end
	end

	-- TODO: grab the diff using lua and push it into a buffer
	--
	-- local my_preview = previewers.new_termopen_previewer {
	-- 	get_command = function(entry, status)
	-- 		vim.print(entry)
	-- 		return { 'git', 'diff', '--no-index', '--', entry.path, '-', '<<<', 'hello code' }
	-- 	end,
	-- }

	pickers
		.new(opts, {
			prompt_title = 'unsaved',
			finder = finders.new_table {
				results = results,
				entry_maker = opts.entry_maker or make_entry.gen_from_file(opts),
			},
			sorter = conf.file_sorter(opts),
			-- TODO: could do something with the diff as preview
			previewer = conf.file_previewer(opts),
			-- previewer = my_preview,
		})
		:find()
end

return {
	unsaved = unsaved,
}
