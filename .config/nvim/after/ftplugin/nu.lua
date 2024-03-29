local vim = vim
local opt = vim.opt

opt.foldmethod = 'expr'
opt.foldexpr = 'nvim_treesitter#foldexpr()'

U.keys(0, {
	{ 'gd', require('codethread.dumbjump').jump, 'Go to def' },
}, { prefix = '' })
