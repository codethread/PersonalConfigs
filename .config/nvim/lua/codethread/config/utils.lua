local M = {}

function M.pipe(functions)
	return function(input)
		local result = input
		for _, func in ipairs(functions) do
			result = func(result)
		end
		return result
	end
end

---@param client string
---@param on_attach fun(client, buffer)
function M.lsp_attach(client, on_attach)
	vim.api.nvim_create_autocmd('LspAttach', {
		callback = function(args)
			local buffer = args.buf
			local current_client = vim.lsp.get_client_by_id(args.data.client_id)
			if client == '*' or client == current_client.name then on_attach(current_client, buffer) end
		end,
	})
end

M.augroups = {}
M.augroups.lsp_formatting = vim.api.nvim_create_augroup('LspFormatting', {})

---@alias ct.Rose 'base' | 'surface' | 'overlay' | 'muted' | 'subtle' | 'text' | 'leaf' | 'love' | 'gold' | 'rose' | 'pine' | 'foam' | 'iris' | 'highlight_low' | 'highlight_med' | 'highlight_high' | 'none' | string

---@class ct.RoseColor : vim.api.keyset.highlight
---@field fg? ct.Rose
---@field bg? ct.Rose

---Apply a set of highlights (supports rose_pine colors)
---This needs to be set as dependency in a lazy.spec e.g:
---```lua
---return {
---	'lukas-reineke/indent-blankline.nvim',
---	dependencies = {
---		U.highlights {
---			IblIndent = { fg = 'overlay' },
---			IblScope = { fg = 'iris' },
---		},
---	},
---	opts = {},
---}
---```
---Under the covers this is setting a dependency on rose pine and adding the highlights there
---@param hls table<string, ct.RoseColor>
---@return table
function M.highlights(hls)
	return {
		'rose-pine/neovim',
		name = 'rose-pine',
		opts = {
			highlight_groups = hls,
		},
	}
end

---Get a single rose-pine color by name
---@param name ct.Rose
---@return string
function M.hl(name)
	local c = require('rose-pine.utilities').parse_color(name)
	if not c or c == 'NONE' then error('no color for ' .. name) end
	return c
end

---Apply a set of highlights (supports rose_pine colors)
---@param table table<string, vim.api.keyset.highlight>
function M.hls(table)
	for hl_group, highlight_settings in pairs(table) do
		local highlights = {}
		for setting, color in pairs(highlight_settings) do
			highlights[setting] = M.hl(color)
		end
		vim.api.nvim_set_hl(0, hl_group, highlights)
	end
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

local function get_visual_selection()
	-- also https://github.com/neovim/neovim/pull/13896#issuecomment-774680224

	-- Yank current visual selection into the 'v' register
	--
	-- Note that this makes no effort to preserve this register
	vim.cmd 'noau normal! "vy"'

	local s = vim.fn.getreg 'v'
	dd(s)
end

local function get_visual()
	local _, ls, cs = unpack(vim.fn.getpos 'v')
	local _, le, ce = unpack(vim.fn.getpos '.')

	-- nvim_buf_get_text requires start and end args be in correct order
	ls, le = math.min(ls, le), math.max(ls, le)
	cs, ce = math.min(cs, ce), math.max(cs, ce)

	local s = vim.api.nvim_buf_get_text(0, ls - 1, cs - 1, le - 1, ce, {})
	for _, value in ipairs(s) do
		dd(value)
	end
end

vim.keymap.set('v', '<localleader>r', get_visual, { buffer = true })
vim.keymap.set('v', '<localleader>e', get_visual_selection, { buffer = true })

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

---Get current file relative to cwd
---@return string
function M.get_current_file()
	local Path = require 'plenary.path'
	local full_path = vim.fn.expand '%:p'
	return Path:new(full_path):make_relative()
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

---Create a keymap, essentially a wrapper around vim.keymap.set, however opts
---can also be a string, in which case it is treated as the 'desc' value
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
---@param filetype number | string | string[] either a number for a buffer or a string for a filetype, if the latter, an autocmd will be created
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

	if type(filetype) == 'string' or type(filetype) == 'table' then
		if type(filetype) == 'table' then
			for _, ft in ipairs(filetype) do
				if type(ft) ~= 'string' then
					vim.notify(
						'filetypes should be strings when passed as arrays, got' .. vim.inspect(ft),
						vim.log.levels.ERROR
					)
					return
				end
			end
		end
		vim.api.nvim_create_autocmd('FileType', {
			pattern = filetype,
			callback = function(ops)
				local ok, wk = pcall(require, 'which-key')
				if not ok then
					vim.notify('needs which key', vim.log.levels.ERROR)
					return
				end

				wk.register(
					bindings,
					vim.tbl_deep_extend('force', base_options, { buffer = ops.buffer }, opts or {})
				)
			end,
		})
	elseif type(filetype) == 'number' then
		local ok, wk = pcall(require, 'which-key')
		if not ok then
			vim.notify('needs which key', vim.log.levels.ERROR)
			return
		end

		wk.register(
			bindings,
			vim.tbl_deep_extend('force', base_options, { buffer = filetype }, opts or {})
		)
	else
		vim.notify(
			'keys is intended for filetype local mappings, pass a filetype or a bufnr',
			vim.log.levels.ERROR
		)
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

---Run a nushell command using `vim.system`
---@overload fun(nu_block: string, opts?: vim.SystemOpts): vim.SystemCompleted
---@overload fun(nu_block: string, opts: vim.SystemOpts, on_exit: fun(res: vim.SystemCompleted)): vim.SystemObj
function M.nush(nu_block, opts, on_exit)
	local cmd =
		{ 'nu', '--no-std-lib', '--no-history', '--error-style=plain', '-c', vim.trim(nu_block) }
	opts = vim.tbl_extend('force', { text = true }, opts or {})
	if on_exit then
		return vim.system(cmd, opts, on_exit)
	else
		return vim.system(cmd, opts):wait()
	end
end

-- function M.nush(nu_block) return "nush '" .. nu_block:gsub('\n', '; ') .. "'" end

---Get a deeply nested field from table `t`
---@param t table
---@param ... string keys to dig into table
---@return any | nil
function M.dig(t, ...)
	for _, k in ipairs { ... } do
		t = t[k]
		if not t then return nil end
	end
	return t
end

---Recurse through a project till a marker is hit or return `nil`
---@param markers string | string[]
---@param opts? { silent?: boolean }
---@return string?
function M.find_lsp_root(markers, opts)
	opts = opts or {}
	---@cast markers string[]
	markers = type(markers) == 'string' and { markers } or markers
	local base = vim.fs.root(0, markers)
	if not base then
		if not opts.silent then
			vim.notify(
				string.format('markers "%s" not found', table.concat(markers, ', ')),
				vim.log.levels.WARN
			)
		end
		return nil
	end
	if #base <= #vim.uv.os_homedir() then
		if not opts.silent then
			vim.notify(
				string.format('found a marker but it was in the home_dir or less "%s"', base),
				vim.log.levels.WARN
			)
		end
		return nil
	end
	return base
end

return M
