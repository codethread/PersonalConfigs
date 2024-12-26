-- use space as the leader key
vim.keymap.set('', '<Space>', '<Nop>')
vim.g.mapleader = ' '

-- use comma as the localleader key
vim.keymap.set('', ',', '<Nop>')
vim.g.maplocalleader = ','

-- config that is always defined
require 'config'
-- disabled vim builtins
require 'disabled'

-- zzz
local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
	-- stylua: ignore
	vim.fn.system { 'git', 'clone', '--filter=blob:none', 'https://github.com/folke/lazy.nvim.git', '--branch=stable', lazypath, }
end
vim.opt.rtp:prepend(lazypath)

require('lazy').setup {
	spec = {
		{ import = 'plugins' },
	},
	dev = {
		path = '~/dev/projects',
		patterns = { 'codethread' },
		fallback = true,
	},
}

-- load all my stuff that isn't a plugin
vim.api.nvim_create_autocmd('User', {
	pattern = 'VeryLazy',
	callback = function() require 'codethread' end,
})

vim.cmd [[colorscheme rose-pine]]
