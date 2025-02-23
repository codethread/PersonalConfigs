local keymapper = require 'codethread.keymaps.keymapper'

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

keymapper.localleader {
	{ ',', 'Reload Luafile', Cmd 'w | luafile %' },
	{ ',', 'Reload block', eval_selection, mode = 'v' },
}
