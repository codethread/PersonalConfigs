-- COMMANDS
-- Remove any disabled or unused plugins
-- :PackerClean
-- Perform `PackerUpdate` and then `PackerCompile`
-- :PackerSync
--
-- stuff is stored at ~/.local/share/nvim
-- cache lives in ~/.cache/nvim/packer

-- Bootstrap Packer
require 'codethread.packer'
local safe_load = require('codethread.utils').safe_load

safe_load('packer', function(packer)
	packer.startup {

		function(use)
			-- Packer can manage itself
			use 'wbthomason/packer.nvim'

			-- rocks
			-- if this hangs, check python is available and https://github.com/wbthomason/packer.nvim/issues/180
			-- https://lunarmodules.github.io/Penlight/classes/pl.List.html
			packer.use_rocks 'penlight'

			-- Things required early
			use 'nvim-lua/plenary.nvim'
			use 'nvim-lua/popup.nvim'
			use 'tpope/vim-obsession'
			use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdateSync' }
			use 'kyazdani42/nvim-web-devicons'

			-- misc
			use { 'wakatime/vim-wakatime', tag = '9.*' }
			use 'nvim-treesitter/playground'
			use 'kevinhwang91/nvim-bqf' -- mainly just like 'o' to close lists

			-- key bindings
			use 'folke/which-key.nvim'
			use 'anuvyklack/hydra.nvim'

			-- colorscheme
			use { 'cormacrelf/dark-notify', run = 'brew install cormacrelf/tap/dark-notify' }
			use 'shaunsingh/nord.nvim' -- classic, but very italic
			use 'folke/tokyonight.nvim' -- snazzy and vibrant, works great with a background terminal image

			-- modeline
			use 'nvim-lualine/lualine.nvim'
			-- project navigation
			use 'farmergreg/vim-lastplace'
			use 'goolord/alpha-nvim'
			use {
				'nvim-telescope/telescope.nvim',
				tag = '0.1.*',
				requires = { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make' },
			}

			--[[
        It sets vim.ui.select to telescope. That means for example that neovim core stuff
        can fill the telescope picker. Example would be lua vim.lsp.buf.code_action().
        --]]
			use 'nvim-telescope/telescope-ui-select.nvim'

			use 'tpope/vim-projectionist'

			use {
				'kyazdani42/nvim-tree.lua',

				requires = 'gabebw/vim-github-link-opener',
			}
			-- use 'tpope/vim-vinegar' -- netrw improvements

			use 'tpope/vim-eunuch' -- unix helpers, :Rename, :Delete

			use 'ThePrimeagen/harpoon'

			-- visual
			-- show colors for css
			use {
				'norcalli/nvim-colorizer.lua',
			}

			use { 'bennypowers/nvim-regexplainer', requires = 'MunifTanjim/nui.nvim' }
			use {
				'ellisonleao/glow.nvim', --[[tag = '0.1.*',]]

				run = 'brew install glow',
			}
			use { 'lukas-reineke/indent-blankline.nvim', tag = 'v2.*' }

			-- project editing
			use 'famiu/bufdelete.nvim' -- delete buffer
			-- automatically creates missing folders
			use 'jghauser/mkdir.nvim'
			-- find/replace
			use {
				'windwp/nvim-spectre',

				run = 'brew install gnu-sed',
			}

			-- file navigation
			use 'nvim-treesitter/nvim-treesitter-context'
			use 'SmiteshP/nvim-navic'
			use { 'kevinhwang91/nvim-ufo', requires = 'kevinhwang91/promise-async', tag = 'v1.*' }

			-- lsp
			use {
				'neovim/nvim-lspconfig',
				requires = {
					'williamboman/mason.nvim',
					'williamboman/mason-lspconfig.nvim',
					'jose-elias-alvarez/typescript.nvim',
				},
			}

			use 'jose-elias-alvarez/null-ls.nvim'
			use 'b0o/schemastore.nvim'
			use 'gbrlsnchs/telescope-lsp-handlers.nvim'
			use {
				'https://git.sr.ht/~whynothugo/lsp_lines.nvim',
			}

			-- completion
			use {
				'hrsh7th/nvim-cmp',
				requires = {
					'hrsh7th/cmp-buffer',
					'hrsh7th/cmp-path',
					'hrsh7th/cmp-cmdline',
					'hrsh7th/cmp-nvim-lsp',
					'onsails/lspkind.nvim',
				},
			}

			use {
				'saadparwaiz1/cmp_luasnip',
				requires = {
					'L3MON4D3/LuaSnip',
					'rafamadriz/friendly-snippets',
				},
			}

			-- windows
			use 'szw/vim-maximizer'
			use 'simrat39/symbols-outline.nvim'
			use { 'rcarriga/nvim-notify', tag = 'v3.*' }
			use 'xorid/swap-split.nvim'

			-- editing
			use 'tpope/vim-rsi' -- readline movement, e.g C-f is forward char
			use {
				'nvim-treesitter/nvim-treesitter-textobjects',
			}

			use 'windwp/nvim-ts-autotag' -- close <div tags, and ciw

			--[[
        Text editing in Neovim with immediate visual feedback: view the effects of any command on your buffer contents live. Preview macros, the :norm command & more!
        --]]
			use { 'smjonas/live-command.nvim', tag = '1.*' }

			use { 'kylechui/nvim-surround', tag = 'v1.*' }
			use 'windwp/nvim-autopairs' -- Autopairs, integrates with both cmp and treesitter
			use 'mbbill/undotree'

			use 'tpope/vim-abolish' -- string  Coercion
			-- Want to turn fooBar into foo_bar? Press crs (coerce to snake_case). MixedCase
			-- (crm), camelCase (crc), snake_case (crs), UPPER_CASE (cru), dash-case (cr-),
			-- dot.case (cr.), space case (cr<space>), and Title Case (crt) are all just 3 keystrokes away.

			use 'AckslD/nvim-neoclip.lua'

			use {
				'JoosepAlviste/nvim-ts-context-commentstring',

				requires = 'tpope/vim-commentary',
			} -- uses tree sitter for comment detection

			use { 'junegunn/vim-easy-align', tag = '2.*' }
			use { 'phaazon/hop.nvim', tag = 'v2.*' }

			-- git
			use {
				'TimUntersberger/neogit',
				requires = {
					'sindrets/diffview.nvim',
				},
			}
			use 'lewis6991/gitsigns.nvim'

			use 'tpope/vim-fugitive'
			-- use("tpope/vim-rhubarb") -- :GBrowse and other git things

			-- langs

			-- help for lua, TODO need to make this work
			-- use 'wsdjeg/luarefvim'
			-- use 'rafcamlet/nvim-luapad'
			use 'milisims/nvim-luaref'
			use 'nanotee/luv-vimdocs'
			use 'folke/neodev.nvim'

			-- terminal
			use { 'akinsho/toggleterm.nvim', tag = 'v2.*' }

			use 'christoomey/vim-tmux-navigator'

			if PACKER_BOOTSTRAP then require('packer').sync() end
		end,
		config = {
			max_jobs = 20,
			-- log = { level = 'info'},
		},
	}
end)

-- Update on save
vim.cmd [[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]]
