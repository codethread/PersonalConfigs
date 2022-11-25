-- TIPS
-- to see raw key
-- go to insert mode, type <C-v> then type, and that key will be shown
--
-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

local utils = require 'codethread.utils'
local map = utils.map
local imap = utils.imap
local nmap = utils.nmap

-- use space as the leader key
map('', '<Space>', '<Nop>')
vim.g.mapleader = ' '
vim.g.maplocalleader = ','

-- emacs movements
imap('<C-f>', '<Right>')
imap('<C-b>', '<Left>')
imap('<A-b>', '<C-o>b')
imap('<A-f>', '<C-o>w')
imap('<C-e>', '<C-o>$')
nmap('<C-e>', '$') -- could probably do something more exciting with this

-- Escape
imap('jk', '<ESC>')

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

" Maximise Window
nnoremap <silent><C-Y> :MaximizerToggle<CR>
vnoremap <silent><C-Y> :MaximizerToggle<CR>gv
inoremap <silent><C-Y> <C-o>:MaximizerToggle<CR>

" MAPS ON COMMANDS I DONT LIKE
" map <C-B>
" map <C-G>
" map <C-Q>
" map <C-Y>
" map <C-P>
]]
