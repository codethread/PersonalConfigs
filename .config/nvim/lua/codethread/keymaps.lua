-- TIPS
-- to see raw key
-- go to insert mode, type <C-v> then type, and that key will be shown
local utils = require 'codethread.utils'
local map = utils.map
local imap = utils.imap

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- use space as the leader key
map('', '<Space>', '<Nop>')
vim.g.mapleader = ' '
vim.g.maplocalleader = ','

imap('<C-f>', '<Right>')
imap('<C-b>', '<Left>')

-- swap ; and :
-- nmap(';', ':')
-- nmap(':', ';')
-- vmap(';', ':')
-- vmap(':', ';')

-- Escape
imap('jk', '<ESC>')
-- nmap('<leader><leader>', ':Telescope find_files<cr>')

vim.cmd [[
" emacs habbits
nnoremap <silent> <C-g> <C-c>
imap <silent> <C-g> <C-c>

nnoremap <silent> <C-h> :wincmd h<CR>
nnoremap <silent> <C-j> :wincmd j<CR>
nnoremap <silent> <C-k> :wincmd k<CR>
nnoremap <silent> <C-l> :wincmd l<CR>

" paste in visual selection without adding to register
xnoremap  <leader>p "_dP

" " Copy to clipboard
vnoremap  <leader>y "+y
nnoremap  <leader>Y "+yg_
nnoremap  <leader>y "+y
nnoremap  <leader>yy "+yy

" center on scroll"
nnoremap <C-d> <C-d>zz
nnoremap <C-u> <C-u>zz

" " Paste from clipboard
nnoremap <leader>v "+p
nnoremap <leader>v "+P
vnoremap <leader>v "+p
vnoremap <leader>v "+P

" keep cursor centered (removed some while using scrolloff)
" nnoremap n nzzzv
" nnoremap N Nzzzv
nnoremap J mzJ`z

" add undo break points on key stroke to make undo more granular
inoremap , ,<c-g>u
inoremap . .<c-g>u
inoremap ( (<c-g>u
inoremap { {<c-g>u

" move text
vnoremap <Down> :m '>+1<CR>gv=gv
vnoremap <Up> :m '<-2<CR>gv=gv

xmap ga <Plug>(EasyAlign)

let @c = 'vi(:EasyAlign */ , /'

" Maximise Window
nnoremap <silent><C-Y> :MaximizerToggle<CR>
vnoremap <silent><C-Y> :MaximizerToggle<CR>gv
inoremap <silent><C-Y> <C-o>:MaximizerToggle<CR>

" MAPS ON COMMANDS I DONT LIKE
nnoremap <silent>_ :NvimTreeToggle<CR>
" map <C-B>
" map <C-G>
" map <C-Q>
nnoremap <C-E> $
" map <C-Y>
" map <C-P>
]]
