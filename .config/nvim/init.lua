do -- setup some common keymaps
	-- use space as the leader key
	vim.keymap.set('', '<Space>', '<Nop>')
	vim.g.mapleader = ' '

	-- use comma as the localleader key
	vim.keymap.set('', ',', '<Nop>')
	vim.g.maplocalleader = ','
end

-- config that is always defined
require 'codethread.config'
-- disabled vim builtins
require 'codethread.disabled'

do -- 💤 bootstrap
	local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
	if not vim.uv.fs_stat(lazypath) then
		--stylua: ignore
		local out = vim.system({ 'git', 'clone', '--filter=blob:none', '--branch=stable', 'https://github.com/folke/lazy.nvim.git', lazypath, }) :wait()
		if out.code ~= 0 then
		--stylua: ignore
			vim.api.nvim_echo({ { 'Failed to clone lazy.nvim:\n', 'ErrorMsg' }, { out.stdout .. out.stderr, 'WarningMsg' }, { '\nPress any key to exit...' }, }, true, {})
			vim.fn.getchar()
			os.exit(1)
		end
	end
	vim.opt.rtp:prepend(lazypath)
end

do -- update print to use snacks
	---@module 'snacks'

	_G.dd = function(...) Snacks.debug.inspect(...) end
	_G.bt = function() Snacks.debug.backtrace() end
	vim.print = _G.dd
end

---@type LazyConfig
local opts = {
	spec = {
		{ import = 'plugins' },
	},
	defaults = {
		version = false, -- always use latest git commit of a plugin
		-- version = "*", -- try installing the latest stable version for plugins that support semver
	},
	dev = {
		path = '~/dev/projects',
		patterns = { 'codethread' },
		fallback = true,
	},
	install = {
		colorscheme = { 'rose-pine' },
	},
}
require('lazy').setup(opts)

-- load all my stuff that isn't a plugin
vim.api.nvim_create_autocmd('User', {
	pattern = 'VeryLazy',
	callback = function() require 'codethread' end,
})
