vim.cmd [[
" show cursor line for inactive buffer to make context switching easier
augroup CursorLine
  au!
  au WinEnter,BufWinEnter * setlocal nocursorline
  au WinLeave * setlocal cursorline
augroup END

augroup JSONChange
  au! BufRead,BufNewFile *.json set filetype=jsonc
augroup END
]]
