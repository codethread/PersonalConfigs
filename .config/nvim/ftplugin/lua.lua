vim.api.nvim_create_autocmd({ 'BufWritePre' }, {
	group = vim.api.nvim_create_augroup('codethread_simple_tbl', { clear = true }),
	callback = function(event) require('codethread.tbl_align').format_table(event.buf) end,
})
