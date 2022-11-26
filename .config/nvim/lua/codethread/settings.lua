local set = vim.opt

set.hidden = true -- Required to keep multiple buffers open multiple buffers
set.encoding = 'utf-8' -- The encoding displayed
set.pumheight = 10 -- Makes popup menu smaller
set.fileencoding = 'utf-8' -- The encoding written to file
set.ruler = true -- Show the cursor position all the time
set.wrap = false -- Display long lines as just one line
-- set.cmdheight = 2 -- More space for displaying messages
set.splitbelow = true -- Horizontal splits will automatically be below
set.splitright = true -- Vertical splits will automatically be to the right
set.conceallevel = 0 -- So that I can see `` in markdown files

set.tabstop = 4 -- Insert 2 spaces for a tab
set.shiftwidth = 0 -- Indent to whatever tabstop is
set.smartindent = true -- Makes indenting smart
set.autoindent = true -- Good auto indent

set.softtabstop = 2 -- Set the behavior of tab
set.smarttab = true -- Makes tabbing smarter will realize you have 2 vs 4
set.expandtab = true -- Converts tabs to spaces

-- set.laststatus = 0 -- Always display the status line
set.relativenumber = true
set.number = true -- Line numbers
set.cursorline = true -- Enable highlighting of the current line
set.showtabline = 2 -- Always show tabs

set.updatetime = 300 -- Faster completion
set.timeoutlen = 500 -- By default timeoutlen is 1000 ms
-- set.formatoptions-=cro                  -- Stop newline continution of comments
-- set.clipboard:append("unnamed") -- Copy paste between vim and everything else
set.signcolumn = 'yes'
-- set.completeopt = "menu,menuone,noselect"
set.completeopt = 'menuone,noselect,preview'
-- always keep some space around the window
set.scrolloff = 8
set.hlsearch = false -- don't keep / highlights after searching
set.showmode = false -- We don't need to see things like -- INSERT -- anymore

vim.cmd [[
  set iskeyword+=-                      	" treat dash separated words as a word text object"
  set mouse=a                             " Enable your mouse
" hi Search gui=undercurl guibg=none guifg=inherit guisp=green "underlineline", "undercurl", "underdot", and "underdash" fall back
]]

-- au! BufWritePost $MYVIMRC source %      " auto source when writing to init.vm alternatively you can run :source $MYVIMRC
