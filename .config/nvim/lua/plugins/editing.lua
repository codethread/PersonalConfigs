---@type conform.FiletypeFormatter
local prettier_format = {
	'prettierd',
	'prettier',
	stop_after_first = true,
}

------Create a table of formatters for conform
------@param formatters conform.FiletypeFormatter
------@param extended { [string]: string }
---local function create_formatters(formatters, extended)
---	-- require('conform')
---
---end

vim.cmd [[
	" paste in visual selection without adding to register
	xnoremap <leader>p "_dP

	" delete but without adding to register
	nnoremap x "_d
	nnoremap X "_D

	" Copy to clipboard
	vnoremap  <leader>y "+y
	nnoremap  <leader>Y "+yg_
	nnoremap  <leader>y "+y
	nnoremap  <leader>yy "+yy

	" Paste from clipboard
	nnoremap <leader>v "+p
	nnoremap <leader>v "+P
	vnoremap <leader>v "+p
	vnoremap <leader>v "+P

	" move text
	vnoremap <Down> :m '>+1<CR>gv=gv
	vnoremap <Up> :m '<-2<CR>gv=gv
]]

---@module "lazy"
---@return LazySpec[]
return {
	'wellle/targets.vim',

	{ 'tpope/vim-rsi', event = { 'InsertEnter', 'CmdlineEnter' } }, -- readline movement, e.g C-f is forward char

	{
		'kylechui/nvim-surround',
		version = 'v1.*',
		event = 'VeryLazy',
		config = true,
	},

	{
		-- Text editing in Neovim with immediate visual feedback: view the effects of any
		-- command on your buffer contents live. Preview macros, the :norm command & more!
		'smjonas/live-command.nvim',
		main = 'live-command',
		opts = {
			-- 	inline_highlighting = false,
			-- break_undo = false,
			commands = {
				Norm = {
					-- :%Norm 0f{ciwlook mum, no hands!
					cmd = 'norm',
				},
				S = {
					-- results
					-- run :%/result/outcome and see bottome text change, using vim-abolish
					-- :S/result{,s} can also be used as just a search
					cmd = 'Subvert',
				},
				G = {
					cmd = 'g',
				},
			},
		},
	},

	-- Want to turn fooBar into foo_bar? Press crs (coerce to snake_case). MixedCase
	-- (crm), camelCase (crc), snake_case (crs), UPPER_CASE (cru), dash-case (cr-),
	-- dot.case (cr.), space case (cr<space>), and Title Case (crt) are all just 3 keystrokes away.
	'tpope/vim-abolish',

	{
		'junegunn/vim-easy-align',
		init = function()
			-- TODO move
			vim.cmd [[
        " nmap ga <Plug>(EasyAlign)
        " xmap ga <Plug>(EasyAlign)
      ]]
			vim.api.nvim_create_user_command(
				'CsvFormat',
				':EasyAlign *,<CR>',
				{ desc = 'Align a csv file' }
			)
		end,
	},

	{ 'xlboy/swap-ternary.nvim' }, -- seems over engineered but works

	{
		'folke/ts-comments.nvim',
		opts = {},
		event = 'VeryLazy',
	},

	{
		-- formatting
		'stevearc/conform.nvim',
		event = { 'BufWritePre' },
		cmd = { 'ConformInfo' },
		init = function()
			vim.api.nvim_create_user_command('Format', function(args)
				local range = nil
				if args.count ~= -1 then
					local end_line = vim.api.nvim_buf_get_lines(0, args.line2 - 1, args.line2, true)[1]
					range = {
						start = { args.line1, 0 },
						['end'] = { args.line2, end_line:len() },
					}
				end
				require('conform').format { async = true, lsp_format = 'fallback', range = range }
			end, { range = true })
		end,
		---@module "conform"
		---@type conform.setupOpts
		opts = {
			formatters_by_ft = {
				lua = { 'stylua' },
				go = { 'goimports', 'gofmt' },
				rust = { 'rustfmt' },

				-- TODO: migrate null-ls
				-- formatting.rustfmt.with {
				-- 	extra_args = function(params)
				-- 		local Path = require 'plenary.path'
				-- 		local cargo_toml = Path:new(params.root .. '/' .. 'Cargo.toml')
				--
				-- 		if cargo_toml:exists() and cargo_toml:is_file() then
				-- 			for _, line in ipairs(cargo_toml:readlines()) do
				-- 				local edition = line:match [[^edition%s*=%s*%"(%d+)%"]]
				-- 				if edition then return { '--edition=' .. edition } end
				-- 			end
				-- 		end
				-- 		-- default edition when we don't find `Cargo.toml` or the `edition` in it.
				-- 		return { '--edition=2021' }
				-- 	end,
				-- },
				--

				javascript = prettier_format,
				javascriptreact = prettier_format,
				typescript = prettier_format,
				typescriptreact = prettier_format,
				css = prettier_format,
				html = prettier_format,
				json = prettier_format,
				jsonc = prettier_format,
				yaml = prettier_format,
				markdown = prettier_format,
				graphql = prettier_format,
				svelte = prettier_format,

				sh = { 'shfmt' },
				bash = { 'shfmt' },
				zsh = { 'shfmt' },

				c = { 'clang_format' },

				proto = { 'buf' },

				-- applied as fallback
				['_'] = { 'trim_whitespace' },
			},
			format_on_save = { -- table must be present
				timeout_ms = 500,
				lsp_format = 'fallback',
				-- by default all formatters will be run, this flips that
				-- and can then opt in in `formatters_by_ft`
				stop_after_first = true,
			},
			formatters = {
				shfmt = {
					prepend_args = { '-i', '2' },
				},
			},
		},
	},
}
