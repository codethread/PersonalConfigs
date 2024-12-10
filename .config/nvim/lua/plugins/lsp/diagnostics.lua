local function set_signs()
	local signs = {
		{ name = 'DiagnosticSignError', text = '' },
		{ name = 'DiagnosticSignWarn', text = '' },
		{ name = 'DiagnosticSignHint', text = '' },
		{ name = 'DiagnosticSignInfo', text = '' },
	}

	for _, sign in ipairs(signs) do
		vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = '' })
	end

	return signs
end

local function format(diagnostic)
	if diagnostic.source == 'eslint_d' then
		return string.format(
			'%s [%s]',
			diagnostic.message,
			-- shows the name of the rule
			diagnostic.code
		)
	end
	return string.format('%s [%s]', diagnostic.message, diagnostic.source)
end

---@module "lazy"
---@return LazySpec[]
return {
	{
		'neovim/nvim-lspconfig',
		opts = function(_, opts)
			opts.diagnostics = {
				severity_sort = true,
				signs = { active = set_signs() },
				underline = true,
				update_in_insert = false,
				virtual_text = { spacing = 4, source = 'if_many', prefix = '●' },
				float = {
					focusable = false,
					style = 'minimal',
					border = 'solid', -- see :h nvim_open_win()
					source = 'always',
					header = '',
					prefix = '',
					format = format,
				},
			}
		end,
	},

	{
		'mfussenegger/nvim-lint',
		event = { 'BufWritePost', 'BufReadPost', 'InsertLeave' },
		cmd = { 'LinterInfo' },
		-- TODO: https://www.lazyvim.org/plugins/linting
		init = function()
			vim.api.nvim_create_user_command('LinterInfo', function()
				local lint = require 'lint'
				local linters = lint.get_running()
				local out = #linters == 0 and '󰦕' or '󱉶 ' .. table.concat(linters, ', ')
				vim.notify(out)
			end, { desc = 'Lint current buffer with info' })

			vim.api.nvim_create_autocmd({ 'BufWritePost', 'BufReadPost', 'InsertLeave' }, {
				group = vim.api.nvim_create_augroup('nvim-lint', { clear = true }),
				callback = require('codethread.fns').debounce(100, function()
					print 'run'
					require('lint').try_lint()
				end),
			})
		end,
		config = function()
			print 'setup'
			require('lint').linters_by_ft = {
				fish = { 'fish' },
				lua = { 'luacheck' },
				bash = { 'shellcheck' },
				sh = { 'shellcheck' },
				proto = { 'buf_lint' },

				-- Use the "*" filetype to run linters on all filetypes.
				-- ['*'] = { 'global linter' },
				-- Use the "_" filetype to run linters on filetypes that don't have other linters configured.
				-- ['_'] = { 'fallback linter' },
				-- ["*"] = { "typos" },
			}
		end,
	},
}
