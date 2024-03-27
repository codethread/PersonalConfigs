local actions = require 'telescope.actions'
local channel = require('plenary.async.control').channel
local conf = require('telescope.config').values
local finders = require 'telescope.finders'
local make_entry = require 'telescope.make_entry'
local pickers = require 'telescope.pickers'
local previewers = require 'telescope.previewers'
local sorters = require 'telescope.sorters'

local M = {}
function M.unsaved(opts)
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

local function get_workspace_symbols_requester(bufnr)
	local cwd = vim.fn.getcwd()
	local cancel = function() end

	return function(prompt)
		local tx, rx = channel.oneshot()
		cancel()
		_, cancel = vim.lsp.buf_request(bufnr, 'workspace/symbol', { query = prompt }, tx)

		-- Handle 0.5 / 0.5.1 handler situation
		local err, res = rx()
		assert(not err, err)

		local locations = vim.lsp.util.symbols_to_items(res or {}, bufnr) or {}

		return vim.tbl_filter(
			function(item) return vim.startswith(item.filename, cwd) end,
			locations
		)
	end
end

--- This isn't really any good
function M.workspace_symbols(opts)
	if not opts.bufnr then opts.bufnr = vim.api.nvim_get_current_buf() end

	pickers
		.new(opts, {
			prompt_title = 'LSP Dynamic Workspace Symbols',
			finder = finders.new_dynamic {
				entry_maker = make_entry.gen_from_lsp_symbols(opts),
				fn = get_workspace_symbols_requester(opts.bufnr),
			},
			previewer = conf.qflist_previewer(opts),
			sorter = sorters.highlighter_only(opts),
			attach_mappings = function(_, map)
				map('i', '<c-space>', actions.to_fuzzy_refine)
				return true
			end,
		})
		:find()
end

return M
