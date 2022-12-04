local set = vim.opt

set.undofile = false
set.swapfile = false
set.backup = false
set.writebackup = false -- This is recommended by coc

vim.cmd [[
if has("persistent_undo")
   let target_path = expand('~/.local/share/nvim/undodir')

    " create the directory and any parent directories
    " if the location does not exist.
    if !isdirectory(target_path)
        call mkdir(target_path, "p", 0700)
    endif

    let &undodir=target_path
endif
]]
