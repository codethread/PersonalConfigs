vim.api.nvim_create_autocmd({ 'BufWritePre' }, {
	group = vim.api.nvim_create_augroup('ctalign', { clear = true }),
	buffer = vim.api.nvim_get_current_buf(),
	callback = function(event) require('codethread.tbl_align').format_table(event.buf) end,
	desc = 'Align keymap tables',
})

--[[stylua: ignore]] --format
U.localleader {
	{ ',', Cmd 'w | luafile %', 'Reload Luafile' },
}
