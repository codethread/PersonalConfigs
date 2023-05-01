require('oil').setup {
	columns = {
		-- 'icon', -- charming but as these can be edited, its annoying
		-- 'permissions',
		-- 'size',
		-- 'mtime',
	},
	use_default_keymaps = true,
	keymaps = {
		['g?'] = 'actions.show_help',
		['<CR>'] = 'actions.select',
		['<C-s>'] = false,
		['<C-h>'] = false,
		['<C-t>'] = false,
		['<TAB>'] = 'actions.preview',
		['<C-c>'] = false,
		['<C-l>'] = false,
		['<left>'] = 'actions.parent',
		['<right>'] = 'actions.select',
		['_'] = false,
		['`'] = false,
		['~'] = 'actions.open_cwd',
		['g.'] = 'actions.toggle_hidden',
		['!'] = 'actions.open_cmdline',
		-- ['<localleader>c'] = 'actions.copy_entry_path', -- using which key for preview
	},
}

local nmap = require('codethread.utils').nmap
nmap('<C-n>', '<cmd>Oil --float<cr>')

U.wk('oil', {
	c = {
		function() require('oil.actions').copy_entry_path.callback() end,
		'copy filepath',
	},
	r = {
		function() require('oil.actions').refresh.callback() end,
		'refresh',
	},
	t = {
		function() require('oil.actions').select_tab.callback() end,
		'open in tab',
	},
	v = {
		function() require('oil.actions').select_vsplit.callback() end,
		'open in vertical',
	},
	s = {
		function() require('oil.actions').select_split.callback() end,
		'open in split',
	},
	['.'] = {
		function() require('oil.actions').tcd.callback() end,
		'make dir PWD',
	},
})

-- turn off netrw and use oil
vim.g.loaded_netrw = 0
vim.g.loaded_netrwPlugin = 0

local function is_github_link(w)
	local bits = vim.split(w, '/')
	return #bits == 2
end

-- replace missing gx with netrw gone
vim.keymap.set('n', 'gx', function()
	local file = vim.fn.expand '<cfile>'
	local Job = require 'plenary.job'

	if vim.startswith(file, 'http') then
		Job:new({
			command = 'open',
			args = { file },
		}):sync()
	elseif is_github_link(file) then
		Job:new({
			command = 'open',
			args = { 'https://github.com/' .. file },
		}):sync()
	else
		print('not a link: ' .. file)
	end
end, {})
