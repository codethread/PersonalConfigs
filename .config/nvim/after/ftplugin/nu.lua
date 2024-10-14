local vim = vim
local opt = vim.opt

local set = vim.opt_local
set.tabstop = 4 -- Insert X spaces for a tab
set.expandtab = false -- Converts tabs to spaces

opt.foldmethod = 'expr'
opt.foldexpr = 'nvim_treesitter#foldexpr()'

U.keys(0, {
	{ 'gd', require('codethread.dumbjump').jump, 'Go to def' },
}, { prefix = '' })
