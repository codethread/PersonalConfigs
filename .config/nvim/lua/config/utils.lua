local M = {}

---@param client string
---@param on_attach fun(client, buffer)
function M.lsp_attach(client, on_attach)
	vim.api.nvim_create_autocmd('LspAttach', {
		callback = function(args)
			local buffer = args.buf
			local current_client = vim.lsp.get_client_by_id(args.data.client_id)
			if client == '*' or client == current_client.name then
				on_attach(current_client, buffer)
			end
		end,
	})
end

M.augroups = {}
M.augroups.lsp_formatting = vim.api.nvim_create_augroup('LspFormatting', {})

function M.highlights(hls)
	return {
		'rose-pine/neovim',
		name = 'rose-pine',
		opts = {
			highlight_groups = hls,
		},
	}
end

---@param tool_list string[] list of tools to install, e.g { tsserver, eslintd }
---@return table
function M.tools_lsp(tool_list)
	return {
		'williamboman/mason-lspconfig.nvim',
		opts = function(_, opts)
			local o = opts or {}
			o.ensure_installed = vim.list_extend(o.ensure_installed or {}, tool_list)
			return o
		end,
	}
end

---@param tool_list string[] list of tools to install, e.g { tsserver, eslintd }
---@return table
function M.tools_null(tool_list)
	return {
		'williamboman/mason-null-ls.nvim',
		opts = function(_, opts)
			local o = opts or {}
			o.ensure_installed = vim.list_extend(o.ensure_installed or {}, tool_list)
			return o
		end,
	}
end

function M.file_exits(path)
	local p = vim.fs.normalize(path)
	return vim.fn.isdirectory(p) == 1
end

function M.get_visual_selection()
	-- also https://github.com/neovim/neovim/pull/13896#issuecomment-774680224

	-- Yank current visual selection into the 'v' register
	--
	-- Note that this makes no effort to preserve this register
	vim.cmd 'noau normal! "vy"'

	return vim.fn.getreg 'v'
end

---get filetype
function M.ft()
	---@diagnostic disable-next-line: undefined-field
	return vim.api.nvim_exec([[echo &ft]], true)
end

---@return number row, number column
function M.current_pos()
	local c = vim.api.nvim_win_get_cursor(0)
	return c[1], c[2]
end

function M.flatten(specss)
	local collection = {}
	for _, plugins in ipairs(specss) do
		for _, plugin in ipairs(plugins) do
			table.insert(collection, plugin)
		end
	end
	return collection
end

function M.project(path, settings)
	if vim.fn.getcwd() == vim.fn.expand(path) then
		return settings
	else
		return {}
	end
end

---pattern match on home or work
---@generic A : any
---@param match { work?: A, home?: A }
---@return A
function M.machine(match)
	local user = os.getenv 'CT_USER'
	return match[user] or {}
end

-----------------------------------------------------------------------------------
-- Keybindings / mappings
-----------------------------------------------------------------------------------

---create a table of options for keymap.set
---@param opts? table
---@return table
function M.map_args(opts)
	return vim.tbl_extend('keep', { silent = true, noremap = true }, opts or {})
end

---Create a keymap, essentially a wrapper around vim.keymap.set, however opts can also be a string, in which case it is treated as the 'desc' value
---@param mode string | table
---@param lhs string
---@param rhs string | function
---@param opts? table | string
function M.keymap(mode, lhs, rhs, opts)
	local options = opts or {}
	if type(options) == 'string' then
		vim.keymap.set(mode, lhs, rhs, M.map_args { desc = opts })
	else
		vim.keymap.set(mode, lhs, rhs, M.map_args(options))
	end
end

---Create local bindings for a buffer or filetype (currently backed by which-key)
---@param filetype string | number either a number for a buffer or a string for a filetype, if the latter, an autocmd will be created
---@param mapping (string | fun())[][]
---@param opts? table
function M.keys(filetype, mapping, opts)
	local bindings = {}

	for _, binding in ipairs(mapping) do
		local lhs, rhs, desc = binding[1], binding[2], binding[3]
		bindings[lhs] = { rhs, desc }
	end

	local base_options = M.map_args {
		mode = 'n',
		prefix = '<localleader>',
	}

	if type(filetype) == 'string' then
		vim.api.nvim_create_autocmd('FileType', {
			pattern = filetype,
			callback = function(ops)
				local ok, wk = pcall(require, 'which-key')
				if not ok then error 'needs which key' end

				wk.register(
					bindings,
					vim.tbl_deep_extend('force', base_options, { buffer = ops.buffer }, opts or {})
				)
			end,
		})
	elseif type(filetype) == 'number' then
		local ok, wk = pcall(require, 'which-key')
		if not ok then error 'needs which key' end

		wk.register(
			bindings,
			vim.tbl_deep_extend('force', base_options, { buffer = filetype }, opts or {})
		)
	else
		error 'keys is intended for filetype local mappings, pass a filetype or a bufnr'
	end
end

function M.command(desc, lhs, rhs, arg_count)
	vim.api.nvim_buf_create_user_command(arg_count or 0, lhs, rhs, { desc = desc })
end

function M.autocmd(events, opts)
	local group = opts.group or vim.api.nvim_create_augroup('codethread', {
		clear = false,
	})
	if opts.group then vim.api.nvim_clear_autocmds { group = group, buffer = opts.buffer } end
	vim.api.nvim_create_autocmd(events, {
		pattern = opts.pattern,
		group = group,
		buffer = opts.buffer,
		callback = opts.fn,
	})
end

--- Run a nushell command using `vim.system`
--- @param nu_block string
--- @param opts vim.SystemOpts? Options
--- @param on_exit? fun(out: vim.SystemCompleted) Called when subprocess exits. When provided, the command runs async
--- @return vim.SystemObj
function M.nush(nu_block, opts, on_exit) return vim.system({ 'nush', nu_block }, opts, on_exit) end

-- function M.nush(nu_block) return "nush '" .. nu_block:gsub('\n', '; ') .. "'" end

return M
