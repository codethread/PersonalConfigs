vim.api.nvim_create_autocmd('FileType', {
	pattern = { 'clojure', 'query' },
	group = vim.api.nvim_create_augroup('Parinfer', {}),
	callback = function()
		vim.api.nvim_buf_set_option(0, 'formatexpr', 'ParinferGetExpr()')
		vim.api.nvim_buf_set_option(0, 'formatprg', 'Parinfer')
		vim.cmd [[ParinferOn]]
	end,
})
vim.g.parinfer_force_balance = true
