-- hello
local fn = vim.fn
local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({
    'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim',
    install_path
  })
  vim.cmd 'packadd packer.nvim'
end

-- Remove any disabled or unused plugins
-- :PackerClean
-- Perform `PackerUpdate` and then `PackerCompile`
-- :PackerSync

return require('packer').startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  -- help for lua, TODO need to make this work
  use 'wsdjeg/luarefvim'
  use 'rafcamlet/nvim-luapad'

  -- colorscheme
  use 'shaunsingh/nord.nvim'
  use 'navarasu/onedark.nvim'

  -- modeline
  use {
    'hoob3rt/lualine.nvim',
    requires = {'kyazdani42/nvim-web-devicons', opt = true}
  }

  -- project navigation
  use {'nvim-telescope/telescope.nvim', requires = {{'nvim-lua/plenary.nvim'}}}
  use {'nvim-telescope/telescope-fzf-native.nvim', run = 'make'}

  use 'tpope/vim-fugitive'
  use 'tpope/vim-rhubarb'
  use 'tpope/vim-vinegar'
  use 'tpope/vim-eunuch'

  -- keybindings
  use {'tjdevries/astronauta.nvim'}

  -- core
  use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}

  -- lsp
  use {'neovim/nvim-lspconfig'}

  use {"jose-elias-alvarez/null-ls.nvim"}

  use {
    "jose-elias-alvarez/nvim-lsp-ts-utils",
    requires = {'nvim-lua/plenary.nvim'}
  }

  -- completion
  use {"hrsh7th/cmp-nvim-lsp"}

  use {"hrsh7th/cmp-buffer"}

  use {"hrsh7th/nvim-cmp"}

  use 'L3MON4D3/LuaSnip'

  -- editing
  use 'tpope/vim-commentary'
  use 'tpope/vim-surround'
  -- use 'windwp/nvim-autopairs' -- buggy
  use 'chun-yang/auto-pairs'

  -- uses tree sitter for comment detection
  use 'JoosepAlviste/nvim-ts-context-commentstring'

end)
