local set = vim.opt

set.shell = 'zsh' -- better for compatibilty
set.termguicolors = true -- adds more colors
set.hidden = true -- Required to keep multiple buffers open multiple buffers
set.encoding = 'utf-8' -- The encoding displayed
set.pumheight = 10 -- Makes popup menu smaller
set.fileencoding = 'utf-8' -- The encoding written to file
set.spellfile = vim.fn.expand '~' .. '/.config/.en.utf-8.add'
set.ruler = true -- Show the cursor position all the time
set.laststatus = 3
set.wrap = false -- Display long lines as just one line
-- set.cmdheight = 2 -- More space for displaying messages
set.splitbelow = true -- Horizontal splits will automatically be below
set.splitright = true -- Vertical splits will automatically be to the right
-- set.splitkeep = 'screen'

set.tabstop = 2 -- Insert X spaces for a tab
set.shiftwidth = 0 -- Indent to whatever tabstop is
set.smartindent = true -- Makes indenting smart
set.autoindent = true -- Good auto indent

set.softtabstop = 2 -- Set the behavior of tab
set.smarttab = true -- Makes tabbing smarter will realize you have 2 vs 4
set.expandtab = true -- Converts tabs to spaces

-- if wanting to show white space in some way
set.listchars = {
	tab = '» ',
	eol = '¬',
	space = '␣',
	extends = '>',
	precedes = '<',
	trail = '~',
}
-- :set list

set.relativenumber = true
set.number = true -- Line numbers
set.cursorline = true -- Enable highlighting of the current line
set.showtabline = 2 -- Always show tabs

set.updatetime = 300 -- Faster completion
set.timeoutlen = 500 -- By default timeoutlen is 1000 ms
-- set.formatoptions-=cro                  -- Stop newline continution of comments
-- set.clipboard:append("unnamed") -- Copy paste between vim and everything else
set.signcolumn = 'yes'
-- always keep some space around the window
set.scrolloff = 4

set.hlsearch = false -- don't keep / highlights after searching
vim.o.smartcase = true

set.showmode = false -- We don't need to see things like -- INSERT -- anymore

-- can use abolish-grep with this
-- e.g. :S /plugin/ *
-- e.g. :S /plugin/ *.lua (globs other than * don't seem to work with ripgrep, but that's probably just my config)
-- set grepprg=rg\ --hidden\ --glob\ '!.git'\ --vimgrep\ --with-filename
-- stolen https://github.com/williamboman/nvim-config/blob/main/after/plugin/options.lua
vim.opt.grepprg = 'rg --vimgrep --no-heading --smart-case'
vim.opt.formatoptions:remove 't'
vim.opt.formatoptions:remove 'o'
vim.opt.grepformat = '%f:%l:%c:%m,%f:%l:%m'

vim.cmd [[
  set iskeyword+=-                      	" treat dash separated words as a word text object"
  set mouse=a                             " Enable your mouse


" hi Search gui=undercurl guibg=none guifg=inherit guisp=green "underlineline", "undercurl", "underdot", and "underdash" fall back
]]

-- au! BufWritePost $MYVIMRC source %      " auto source when writing to init.vm alternatively you can run :source $MYVIMRC

vim.filetype.add {
	pattern = {
		['.*'] = {
			function(_, bufnr)
				local content = vim.api.nvim_buf_get_lines(bufnr, 0, 1, false)[1] or ''
				if vim.regex([[^#!.*bun]]):match_str(content) ~= nil then return 'typescript' end
			end,
			{ priority = -math.huge },
		},
	},
	extension = {
		nuon = 'nu',
	},
}
