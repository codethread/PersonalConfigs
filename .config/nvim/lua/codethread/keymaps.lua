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

-- for folds see ufo.lua

-- use space as the leader key
map('', '<Space>', '<Nop>')
vim.g.mapleader = ' '
vim.g.maplocalleader = ','

-- Escape
imap('jk', '<ESC>')

nmap('ZQ', '<cmd>qa!<cr>') -- default is q!

nmap('<M-s>', '<cmd>w<cr>') -- alt or cmd on macos (terminal dependent, works with kitty)
nmap('<C-q>', function()
	for _, win in ipairs(vim.api.nvim_tabpage_list_wins(vim.api.nvim_get_current_tabpage())) do
		if vim.fn.getwinvar(win, '&syntax') == 'qf' then
			vim.cmd.cclose()
			return
		end
	end
	vim.cmd.copen()
end)

vim.cmd [[
nnoremap <silent> <C-h> :wincmd h<CR>
nnoremap <silent> <C-j> :wincmd j<CR>
nnoremap <silent> <C-k> :wincmd k<CR>
nnoremap <silent> <C-l> :wincmd l<CR>

" paste in visual selection without adding to register
xnoremap <leader>p "_dP

" delete but without adding to register
nnoremap x "_d
nnoremap X "_D

" Copy to clipboard
vnoremap  <leader>y "+y
nnoremap  <leader>Y "+yg_
nnoremap  <leader>y "+y
nnoremap  <leader>yy "+yy

" center on scroll
" kind of prefer just using scroll off
" nnoremap <C-d> <C-d>zz
" nnoremap <C-u> <C-u>zz

" Paste from clipboard
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
" map <C-B> - done
" map <C-G>
" map <C-Q> - done
" map <C-Y> - done
" map <C-P>
" nmap L
" nmap H
" nmap M
]]
