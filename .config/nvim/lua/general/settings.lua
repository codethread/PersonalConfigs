local set = vim.opt

-- Set the behavior of tab
set.softtabstop = 2

set.relativenumber = true

set.hidden = true -- Required to keep multiple buffers open multiple buffers
-- set.nowrap = true -- Display long lines as just one line
set.encoding = 'utf-8' -- The encoding displayed
set.pumheight = 10 -- Makes popup menu smaller
set.fileencoding = 'utf-8' -- The encoding written to file
set.ruler = true -- Show the cursor position all the time
set.cmdheight = 2 -- More space for displaying messages
-- set.iskeyword+=-                      	-- treat dash separated words as a word text object"
-- set.mouse=a                             -- Enable your mouse
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
-- set.noshowmode = true -- We don't need to see things like -- INSERT -- anymore
-- set.nobackup = true -- This is recommended by coc
-- set.nowritebackup = true -- This is recommended by coc
set.updatetime = 300 -- Faster completion
set.timeoutlen = 500 -- By default timeoutlen is 1000 ms
-- set.formatoptions-=cro                  -- Stop newline continution of comments
set.clipboard:append('unnamedplus') -- Copy paste between vim and everything else
-- set.autochdir = true -- Your working directory will always be the same as your working directory
--
set.completeopt = 'menu,menuone,noselect'
set.termguicolors = true -- adds more colors

-- au! BufWritePost $MYVIMRC source %      " auto source when writing to init.vm alternatively you can run :source $MYVIMRC

