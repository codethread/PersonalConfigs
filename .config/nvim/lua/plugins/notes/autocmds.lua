vim.api.nvim_create_autocmd({ 'BufReadPre' }, {
	group = vim.api.nvim_create_augroup('codethread_obsidian', { clear = true }),
	pattern = '/Users/codethread/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes/*',
	callback = function()
		local wk = require 'which-key'
		-- wk.register(mappings, opts)
	end,
})
