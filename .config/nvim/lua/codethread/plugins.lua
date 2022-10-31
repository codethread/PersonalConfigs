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

local function safe_load(lib, fn)
	local status_ok, loaded_lib = pcall(require, lib)
	if not status_ok then
		print('could not load ' .. lib)
		return
	end
	fn(loaded_lib)
end

safe_load('packer', function(packer)
	packer.startup {
		function(use)
			-- Packer can manage itself
			use { 'wbthomason/packer.nvim' }

			-- rocks
			-- if this hangs, check python is available and https://github.com/wbthomason/packer.nvim/issues/180
			-- https://lunarmodules.github.io/Penlight/classes/pl.List.html
			packer.use_rocks 'penlight'

			-- Things required early
			use { 'nvim-lua/plenary.nvim' }
			use { 'nvim-lua/popup.nvim' }
			use { 'tpope/vim-obsession' }
			use {
				'nvim-treesitter/nvim-treesitter',
				run = function() require('nvim-treesitter.install').update { with_sync = true } end,
			}

			-- misc
			use { 'wakatime/vim-wakatime', tag = '9.*' }
			use { 'nvim-treesitter/playground' }

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
			use {
				'nvim-telescope/telescope.nvim',
				tag = '0.1.*',
				requires = {
					'nvim-telescope/telescope-fzf-native.nvim',
					run = 'make',
				},
			}
			use {
				--[[
        It sets vim.ui.select to telescope. That means for example that neovim core stuff
        can fill the telescope picker. Example would be lua vim.lsp.buf.code_action().
        --]]
				'nvim-telescope/telescope-ui-select.nvim',
			}

			use { 'tpope/vim-projectionist' }

			use {
				'kyazdani42/nvim-tree.lua',
				commit = '65c2ba895213c3641fc58dd33bc7a44423a6cdbe',
				-- commit = '72858986f9de019dc0e151c76090de29954081f0',
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

			use {
				'nvim-treesitter/nvim-treesitter-context',
				config = function()
					require('treesitter-context').setup {
						max_lines = 2, -- How many lines the window should span. Values <= 0 mean no limit.
						trim_scope = 'inner', -- Which context lines to discard if `max_lines` is exceeded. Choices: 'inner', 'outer'
					}
				end,
			}
			use {
				'SmiteshP/nvim-navic',
				commit = '94bf6fcb1dc27bdad230d9385da085e72c390019',
			}
			use {
				'kevinhwang91/nvim-ufo',
				requires = 'kevinhwang91/promise-async',
				tag = 'v1.*',
			}

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
			use 'milisims/nvim-luaref'

			-- terminal
			use { 'akinsho/toggleterm.nvim', tag = 'v2.*' }

			use {
				'christoomey/vim-tmux-navigator',
				config = function() require 'codethread.plugins.vim-tmux-navigator' end,
			}

			if PACKER_BOOTSTRAP then require('packer').sync() end
		end,
		config = {
			max_jobs = 10,
			-- log = { level = 'info'},
			display = {
				-- open_fn = function() return require('packer.util').float { border = 'rounded' } end,
			},
		},
	}
end)

-- Update on save
-- vim.cmd [[
--   augroup packer_user_config
--     autocmd!
--     autocmd BufWritePost plugins.lua source <afile> | PackerSync
--   augroup end
-- ]]
