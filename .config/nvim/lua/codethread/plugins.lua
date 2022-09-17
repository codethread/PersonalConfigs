-- COMMANDS
-- Remove any disabled or unused plugins
-- :PackerClean
-- Perform `PackerUpdate` and then `PackerCompile`
-- :PackerSync
--
-- stuff is stored at ~/.local/share/nvim
-- cache lives in ~/.cache/nvim/packer

-- Bootstrap Packer
require 'codethread.plugins_init'

-- protected load
local status_ok, packer = pcall(require, 'packer')
if not status_ok then
	print 'could not load packer'
	return
end

packer.startup {
	function(use)
		-- Packer can manage itself
		use {
			'wbthomason/packer.nvim',
			commit = '671076b3a81e5033a866ca769749e75a827784ef',
		}

		-- rocks
		-- if this hangs, check python is available
		-- https://lunarmodules.github.io/Penlight/classes/pl.List.html
		packer.use_rocks 'penlight'

		-- Things required early
		use { 'nvim-lua/plenary.nvim', commit = '968a4b9afec0c633bc369662e78f8c5db0eba249' }
		use { 'nvim-lua/popup.nvim', commit = 'b7404d35d5d3548a82149238289fa71f7f6de4ac' }
		use { 'tpope/vim-obsession' }
		use {
			'nvim-treesitter/nvim-treesitter',
			run = function() require('nvim-treesitter.install').update { with_sync = true } end,
			commit = '67fb8939ff1f7e29659f5c4efe50a5689e3458bc',
		}

		-- misc
		use { 'wakatime/vim-wakatime', tag = '9.*' }
		use { 'nvim-treesitter/playground', commit = 'ce7e4b757598f1c785ed0fd94fc65959acd7d39c' }

		-- key bindings
		use { 'folke/which-key.nvim', commit = 'bd4411a2ed4dd8bb69c125e339d837028a6eea71' }
		use { 'anuvyklack/hydra.nvim', commit = '7a471169f2fd577e8893b95a0253dce7b9abf96f' }

		-- colorscheme
		use { 'cormacrelf/dark-notify', run = 'brew install cormacrelf/tap/dark-notify' }
		-- use({ "shaunsingh/nord.nvim" })
		use { 'folke/tokyonight.nvim' }

		-- modeline
		use {
			'nvim-lualine/lualine.nvim',
			requires = { 'kyazdani42/nvim-web-devicons', opt = true },
			commit = '5113cdb32f9d9588a2b56de6d1df6e33b06a554a',
		}
		-- project navigation
		use { 'farmergreg/vim-lastplace' }
		use {
			'goolord/alpha-nvim',
			config = function() require 'codethread.dashboard' end,
			commit = '95d522b2056657eb9968411ef801f51af86fc839',
		}
		use { 'nvim-telescope/telescope.nvim', commit = 'b98b9a93c67cb999493ccdc602e711c8a7a98d64' }
		use {
			'nvim-telescope/telescope-fzf-native.nvim',
			run = 'make',
			commit = '6a33ecefa9b3d9ade654f9a7a6396a00c3758ca6',
		}
		use { 'tpope/vim-projectionist' }
		use { 'nvim-telescope/telescope-ui-select.nvim' }
		use {
			'lewis6991/spellsitter.nvim',
			commit = 'c1b318f8b959e015f5cc7941624d1ca0f84705dd',
			config = function() require('spellsitter').setup() end,
		}

		use {
			'kyazdani42/nvim-tree.lua',
			commit = '72858986f9de019dc0e151c76090de29954081f0',
			config = function() require 'codethread.plugins.nvim-tree' end,
			-- optional, for file icon
			requires = { 'kyazdani42/nvim-web-devicons' },
		}

		use 'tpope/vim-vinegar' -- netrw improvements

		use 'tpope/vim-eunuch' -- unix helpers, :Rename, :Delete

		-- visual
		use {
			'norcalli/nvim-colorizer.lua', -- show colors for css
			commit = '36c610a9717cc9ec426a07c8e6bf3b3abcb139d6',
			config = function() require('colorizer').setup { 'css', 'scss', 'html', 'svelte', 'lua' } end,
		}

		use {
			'bennypowers/nvim-regexplainer',
			config = function() require('regexplainer').setup {} end,
			requires = {
				'nvim-treesitter/nvim-treesitter',
				'MunifTanjim/nui.nvim',
			},
		}
		use {
			'ellisonleao/glow.nvim',
			-- tag = '0.1.*',
			commit = '764527caeb36cd68cbf3f6d905584750cb02229d',
			run = 'brew install glow',
			config = function() require 'codethread.plugins.glow' end,
		}
		use {
			'lukas-reineke/indent-blankline.nvim',
			config = function() require 'codethread.plugins.indent-blankline' end,
			tag = 'v2.*',
		}

		-- project editing
		use 'famiu/bufdelete.nvim' -- delete buffer
		use { -- automatically creates missing folders
			'jghauser/mkdir.nvim',
			commit = '01261650382bef195dab8ac39344234b57914f09',
		}

		use { -- find/replace
			'windwp/nvim-spectre',
			run = 'brew install gnu-sed',
			commit = 'b1a084c05bf6cf32a3b55196e5cde44bb94422fb',
		}

		-- file navigation

		-- use({
		-- 	"nvim-treesitter/nvim-treesitter-context",
		-- 	commit = "0d086d23c0742404e9bd52977712619a621c3da9",
		-- 	config = function()
		-- 		require("treesitter-context").setup({})
		-- 	end,
		-- })
		use {
			'SmiteshP/nvim-navic',
			commit = '94bf6fcb1dc27bdad230d9385da085e72c390019',
		}

		-- lsp
		use 'neovim/nvim-lspconfig'
		use 'williamboman/nvim-lsp-installer'
		use 'jose-elias-alvarez/null-ls.nvim'
		use 'jose-elias-alvarez/nvim-lsp-ts-utils'
		use 'b0o/schemastore.nvim'
		use 'gbrlsnchs/telescope-lsp-handlers.nvim'
		use {
			'https://git.sr.ht/~whynothugo/lsp_lines.nvim',
			commit = 'db67e94c813aae166c3d2f119ea7d2e85164922a',
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
		use {
			'rcarriga/nvim-notify',
			tag = 'v3.*',
			config = function()
				local notify = require 'notify'
				notify.setup {
					background_colour = '#000000',
					max_width = 100,
					stages = 'slide',
					timeout = 500,
				}
				vim.notify = notify
			end,
			-- requires = 'codethread.spinner',
		}

		-- editing
		use {
			'nvim-treesitter/nvim-treesitter-textobjects',
			commit = '40f20e6788e6ce850802cbd2ca029fbb66b5d043',
		}

		use 'windwp/nvim-ts-autotag' -- close <div tags, and ciw

		use {
			'kylechui/nvim-surround',
			config = function() require('nvim-surround').setup {} end,
			tag = 'v1.*',
		}
		use 'windwp/nvim-autopairs' -- Autopairs, integrates with both cmp and treesitter
		use 'mbbill/undotree'
		use 'tpope/vim-abolish' -- string  Coercion
		-- Want to turn fooBar into foo_bar? Press crs (coerce to snake_case). MixedCase
		-- (crm), camelCase (crc), snake_case (crs), UPPER_CASE (cru), dash-case (cr-),
		-- dot.case (cr.), space case (cr<space>), and Title Case (crt) are all just 3 keystrokes away.

		use {
			'AckslD/nvim-neoclip.lua',
			commit = '74af02e289b3ea465bc8a4d7b9b83adc4e4b8c06',
			requires = { 'nvim-telescope/telescope.nvim' },
			config = function() require('neoclip').setup() end,
		}

		use {
			'JoosepAlviste/nvim-ts-context-commentstring',
			commit = '4befb8936f5cbec3b726300ab29edacb891e1a7b',
			requires = { 'tpope/vim-commentary' },
		} -- uses tree sitter for comment detection

		use { 'junegunn/vim-easy-align', tag = '2.*' }
		use { 'phaazon/hop.nvim', tag = 'v2.*' }

		-- git
		use {
			'TimUntersberger/neogit',
			config = function()
				require('neogit').setup {
					integrations = {
						diffview = true,
					},
				}
			end,
			requires = {
				'sindrets/diffview.nvim',
				commit = 'b31fafb71f35e4f2a4bd95481ff7d59b1caae387',
				-- commit = '16c3985581ee65bccdfbebbe014b24a01adc7d1f',
			},
		}
		use 'lewis6991/gitsigns.nvim'

		use 'tpope/vim-fugitive'
		-- use("tpope/vim-rhubarb") -- :GBrowse and other git things

		-- langs

		-- help for lua, TODO need to make this work
		-- use 'wsdjeg/luarefvim'
		-- use 'rafcamlet/nvim-luapad'

		-- terminal
		use { 'akinsho/toggleterm.nvim', tag = 'v2.*' }

		use {
			'christoomey/vim-tmux-navigator',
			config = function()
				vim.cmd [[
		" Disable tmux navigator when zooming the Vim pane
		let g:tmux_navigator_disable_when_zoomed = 1

		let g:tmux_navigator_no_mappings = 1
		nnoremap <silent> <C-h> :TmuxNavigateLeft<cr>
		nnoremap <silent> <C-j> :TmuxNavigateDown<cr>
		nnoremap <silent> <C-k> :TmuxNavigateUp<cr>
		nnoremap <silent> <C-l> :TmuxNavigateRight<cr>
		]]
			end,
		}

		if PACKER_BOOTSTRAP then require('packer').sync() end
	end,
	config = {
		display = {
			open_fn = function() return require('packer.util').float { border = 'rounded' } end,
		},
	},
}

-- Update on save
-- vim.cmd [[
--   augroup packer_user_config
--     autocmd!
--     autocmd BufWritePost plugins.lua source <afile> | PackerSync
--   augroup end
-- ]]
