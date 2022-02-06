local utils = require("utils")
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

-- swap ; and :
-- nmap(';', ':')
-- nmap(':', ';')
-- vmap(';', ':')
-- vmap(':', ';')

-- Escape
map("i", "jk", "<ESC>")
-- nmap('<leader><leader>', ':Telescope find_files<cr>')
