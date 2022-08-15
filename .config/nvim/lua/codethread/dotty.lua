if vim.fn.executable 'dotty' then
	vim.api.nvim_create_autocmd({ 'BufWritePost', 'BufFilePost', 'BufWipeout', 'VimLeavePre' }, {
		callback = function(opts)
			if opts.file == nil then return end

			vim.fn.jobstart({ 'dotty', 'test', opts.file }, {
				on_exit = function(_, code)
					if code ~= 0 then return end

					vim.notify('Running...', 'info', {
						title = 'Dotty',
						hide_from_history = true,
						timeout = 0,
					})

					-- add error handling later
					vim.fn.jobstart { 'dotty', 'setup' }
				end,
			})
		end,
		group = vim.api.nvim_create_augroup('Dotty', { clear = true }),
		pattern = '*/PersonalConfigs/*',
	})
else
	vim.notify('No Dotty bin, you may need to brew install', 'warn', {
		title = 'Dotty',
	})
end
