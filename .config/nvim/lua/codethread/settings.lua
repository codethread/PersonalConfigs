local set = vim.opt

-- Set the behavior of tab
set.softtabstop = 2

set.relativenumber = true

set.hidden = true -- Required to keep multiple buffers open multiple buffers
set.encoding = "utf-8" -- The encoding displayed
set.pumheight = 10 -- Makes popup menu smaller
set.fileencoding = "utf-8" -- The encoding written to file
set.ruler = true -- Show the cursor position all the time
set.cmdheight = 2 -- More space for displaying messages
set.splitbelow = true -- Horizontal splits will automatically be below
set.splitright = true -- Vertical splits will automatically be to the right
set.conceallevel = 0 -- So that I can see `` in markdown files
set.tabstop = 2 -- Insert 2 spaces for a tab
set.shiftwidth = 2 -- Change the number of space characters inserted for indentation
set.smarttab = true -- Makes tabbing smarter will realize you have 2 vs 4
set.expandtab = true -- Converts tabs to spaces
set.smartindent = true -- Makes indenting smart
set.autoindent = true -- Good auto indent
set.laststatus = 0 -- Always display the status line
set.number = true -- Line numbers
set.cursorline = true -- Enable highlighting of the current line
-- set.background=dark                     -- tell vim what the background color looks like
set.showtabline = 2 -- Always show tabs
set.updatetime = 300 -- Faster completion
set.timeoutlen = 500 -- By default timeoutlen is 1000 ms
-- set.formatoptions-=cro                  -- Stop newline continution of comments
set.clipboard:append("unnamedplus") -- Copy paste between vim and everything else
set.signcolumn = "yes"
-- set.completeopt = "menu,menuone,noselect"
set.completeopt = "menuone,noselect,preview"
set.termguicolors = true -- adds more colors

vim.cmd([[
  set foldlevel=99
  set foldmethod=expr
  set foldexpr=nvim_treesitter#foldexpr()
  set noshowmode " We don't need to see things like -- INSERT -- anymore
  set nobackup " This is recommended by coc
  set nowritebackup " This is recommended by coc
  set nowrap " Display long lines as just one line
  set iskeyword+=-                      	" treat dash separated words as a word text object"
  set mouse=a                             " Enable your mouse
]])

-- au! BufWritePost $MYVIMRC source %      " auto source when writing to init.vm alternatively you can run :source $MYVIMRC
