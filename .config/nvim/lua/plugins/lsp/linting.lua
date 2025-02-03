---@return LazySpec[]
return {
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
				callback = require('codethread.fns').debounce(
					100,
					function() require('lint').try_lint() end
				),
			})
		end,
		config = function()
			require('lint').linters_by_ft = {
				fish = { 'fish' },
				-- lua = { 'luacheck' },
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
