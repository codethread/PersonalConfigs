-- config that is always defined
require 'config'
-- disabled vim builtins
require 'disabled'

-- zzz
local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
    -- stylua: ignore
    vim.fn.system { 'git', 'clone', '--filter=blob:none', 'https://github.com/folke/lazy.nvim.git', '--branch=stable',
        lazypath, }
end
vim.opt.rtp:prepend(lazypath)

require('lazy').setup {
	spec = {
		{ import = 'plugins' },
	},
}

-- load all my stuff that isn't a plugin
vim.api.nvim_create_autocmd('User', {
	pattern = 'VeryLazy',
	callback = function() require 'codethread' end,
})

vim.cmd [[colorscheme rose-pine]]
