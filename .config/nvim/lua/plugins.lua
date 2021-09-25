
-- hello
local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
	fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
	vim.cmd 'packadd packer.nvim'
end

return require('packer').startup(function()
	-- Packer can manage itself
	use 'wbthomason/packer.nvim'

	use 'shaunsingh/nord.nvim'

	use {
		'hoob3rt/lualine.nvim',
		requires = {'kyazdani42/nvim-web-devicons', opt = true}
	}

	use {
		'nvim-telescope/telescope.nvim',
		requires = { {'nvim-lua/plenary.nvim'} }
	}

	use {
		'nvim-telescope/telescope-fzf-native.nvim',
		run = 'make' 
	}

  use {
     'tjdevries/astronauta.nvim'
  }

  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate'
  }

  use {
    'neovim/nvim-lspconfig'
  }

  use {
    "jose-elias-alvarez/null-ls.nvim" 
  }

  use {
     "jose-elias-alvarez/nvim-lsp-ts-utils",
     requires = { 'nvim-lua/plenary.nvim'}
  }

end)
