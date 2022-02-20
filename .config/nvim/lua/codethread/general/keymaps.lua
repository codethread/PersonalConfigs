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

nnoremap <silent> - :NvimTreeFindFile<CR>

nnoremap <silent> <C-h> :wincmd h<CR>
nnoremap <silent> <C-j> :wincmd j<CR>
nnoremap <silent> <C-k> :wincmd k<CR>
nnoremap <silent> <C-l> :wincmd l<CR>
]])
