vim.api.nvim_create_autocmd({ 'BufWritePre' }, {
	group = vim.api.nvim_create_augroup('ctalign', { clear = true }),
	buffer = vim.api.nvim_get_current_buf(),
	callback = function(event) require('codethread.tbl_align').format_table(event.buf) end,
	desc = 'Align keymap tables',
})

local function eval_selection()
	local lines = U.get_visual_selection()
	vim.cmd [[norm ]]
	assert(loadstring(table.concat(lines)))()
end

--[[stylua: ignore]] --format
U.localleader {
	{ ',', Cmd 'w | luafile %', 'Reload Luafile' },
	{ ',', eval_selection     , 'Reload block'  , mode = 'v' },
}
