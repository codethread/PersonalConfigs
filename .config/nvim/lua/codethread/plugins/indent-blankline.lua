local colors = require('tokyonight.colors').setup {}
vim.cmd [[let g:indentLine_char = '┊']]
vim.cmd('hi IndentBlanklineChar guifg=' .. colors.fg_gutter)
vim.cmd('hi IndentBlanklineContextChar guifg=' .. colors.magenta)

require('indent_blankline').setup {
	show_current_context = true,
	show_current_context_start = false,
}
