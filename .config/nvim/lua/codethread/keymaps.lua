-- TIPS
-- to see raw key
-- go to insert mode, type <C-v> then type, and that key will be shown
local utils = require("codethread.utils")
local map = utils.map
local nmap = utils.nmap
local vmap = utils.vmap

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- use space as the leader key
map("", "<Space>", "<Nop>")
vim.g.mapleader = " "
vim.g.maplocalleader = ","

map("i", "<C-f>", "<Right>")
map("i", "<C-b>", "<Left>")

-- swap ; and :
-- nmap(';', ':')
-- nmap(':', ';')
-- vmap(';', ':')
-- vmap(':', ';')

-- Escape
map("i", "jk", "<ESC>")
-- nmap('<leader><leader>', ':Telescope find_files<cr>')

vim.cmd([[
" emacs habbits
nnoremap <silent> <C-g> <C-c>
imap <silent> <C-g> <C-c>

nnoremap <silent> <C-h> :wincmd h<CR>
nnoremap <silent> <C-j> :wincmd j<CR>
nnoremap <silent> <C-k> :wincmd k<CR>
nnoremap <silent> <C-l> :wincmd l<CR>

" " Copy to clipboard
vnoremap  <leader>y  "+y
nnoremap  <leader>Y  "+yg_
nnoremap  <leader>y  "+y
nnoremap  <leader>yy  "+yy

" " Paste from clipboard
nnoremap <leader>v "+p
nnoremap <leader>v "+P
vnoremap <leader>v "+p
vnoremap <leader>v "+P

" keep cursor centered
nnoremap n nzzzv
nnoremap N Nzzzv
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
]])
