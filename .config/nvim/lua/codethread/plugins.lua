-- COMMANDS
-- Remove any disabled or unused plugins
-- :PackerClean
-- Perform `PackerUpdate` and then `PackerCompile`
-- :PackerSync
--
-- stuff is stored at ~/.local/share/nvim
-- cache lives in ~/.cache/nvim/packer

-- Bootstrap Packer, don't move this from the top
local p = require 'codethread.packer'
local packer_bootstrap = p.ensure_packer()
local safe_load = require('codethread.utils').safe_load

safe_load('packer', function(packer)
	packer.startup {
		function(use)
			local use_local = p.use_local_partial(use)
			local use_rocks = packer.use_rocks

			-- Packer can manage itself
			use 'wbthomason/packer.nvim'

			-- rocks
			-- if this hangs, check python is available and https://github.com/wbthomason/packer.nvim/issues/180
			-- https://lunarmodules.github.io/Penlight/classes/pl.List.html
			use_rocks 'penlight'
			use_rocks 'matcher_combinators'

			-- Things required early
			use 'nvim-lua/plenary.nvim'
			use 'nvim-lua/popup.nvim'
			use 'kyazdani42/nvim-web-devicons'
			use 'farmergreg/vim-lastplace'

			-- treesitter
			use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdateSync' }
			use 'nvim-treesitter/nvim-treesitter-context'
			use 'nvim-treesitter/nvim-treesitter-textobjects'
			use 'nvim-treesitter/playground'
			use 'windwp/nvim-ts-autotag' -- close <div tags, and ciw
			use { 'JoosepAlviste/nvim-ts-context-commentstring', requires = 'tpope/vim-commentary' }
			use { 'kevinhwang91/nvim-ufo', requires = 'kevinhwang91/promise-async', tag = 'v1.*' }

			-- telescope
			use {
				'nvim-telescope/telescope.nvim',
				tag = '0.1.x',
				requires = { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make' },
			}
			use 'nvim-telescope/telescope-ui-select.nvim'
			use 'LukasPietzschmann/telescope-tabs'

			--------------------------
			-- lore friendly improvements
			--------------------------
			use 'AckslD/nvim-neoclip.lua'
			use 'famiu/bufdelete.nvim' -- delete buffer
			use 'goolord/alpha-nvim'
			use 'jghauser/mkdir.nvim'
			use 'mbbill/undotree'
			use 'norcalli/nvim-colorizer.lua'
			use 'szw/vim-maximizer'
			use 'tpope/vim-eunuch' -- unix helpers, :Rename, :Delete
			use 'tpope/vim-obsession'
			use 'xorid/swap-split.nvim'
			use { 'phaazon/hop.nvim', tag = 'v2.*' }
			use { 'shortcuts/no-neck-pain.nvim', tag = '*' }

			-- editing
			use 'tpope/vim-rsi' -- readline movement, e.g C-f is forward char
			-- use 'windwp/nvim-autopairs' -- Autopairs, integrates with both cmp and treesitter
			use 'wellle/targets.vim'
			use { 'junegunn/vim-easy-align', tag = '2.*' }
			use { 'kylechui/nvim-surround', tag = 'v1.*' }
			use { 'smjonas/live-command.nvim', tag = '1.*' } -- Text editing in Neovim with immediate visual feedback: view the effects of any command on your buffer contents live. Preview macros, the :norm command & more!
			use { 'windwp/nvim-spectre', run = 'brew install gnu-sed' } -- find/replace
			use 'tpope/vim-abolish' -- string  Coercion
			--[[                    Want to turn fooBar into foo_bar? Press crs (coerce to snake_case). MixedCase
                                    (crm), camelCase (crc), snake_case (crs), UPPER_CASE (cru), dash-case (cr-),
                                    dot.case (cr.), space case (cr<space>), and Title Case (crt) are all just 3 keystrokes away. --]]
			use_local '~/dev/projects/qmk.nvim'

			-- navigation
			use 'ThePrimeagen/harpoon'
			use 'kevinhwang91/nvim-bqf' -- mainly just like 'o' to close lists
			use { 'bennypowers/nvim-regexplainer', requires = 'MunifTanjim/nui.nvim' }
			use { 'kyazdani42/nvim-tree.lua', requires = 'gabebw/vim-github-link-opener' }
			use { 'lukas-reineke/indent-blankline.nvim', tag = 'v2.*' }
			use { 'rcarriga/nvim-notify', tag = 'v3.*' }
			use { 'wakatime/vim-wakatime', tag = '9.*' }
			use {
				'folke/twilight.nvim',
				config = function()
					-- TODO: steal code and adjust to highlight blocks in js
					require('twilight').setup {
						expand = {
							'function',
							'method',
							'table',
							'if_statement',
							'lexical_declaration',
						},
						-- your configuration comes here
						-- or leave it empty to use the default settings
						-- refer to the configuration section below
					}
				end,
			}

			-- key bindings
			use 'folke/which-key.nvim'
			use 'anuvyklack/hydra.nvim'

			-- colorscheme
			use { 'cormacrelf/dark-notify', run = 'brew install cormacrelf/tap/dark-notify' }
			use 'shaunsingh/nord.nvim' -- classic, but very italic
			use 'folke/tokyonight.nvim' -- snazzy and vibrant, works great with a background terminal image
			use 'nvim-lualine/lualine.nvim'
			use 'fgheng/winbar.nvim'

			-- lsp
			use {
				'neovim/nvim-lspconfig',
				requires = {
					'williamboman/mason.nvim',
					'williamboman/mason-lspconfig.nvim',
				},
			}

			use 'arkav/lualine-lsp-progress'
			use 'SmiteshP/nvim-navic'
			use 'b0o/schemastore.nvim'
			use 'gbrlsnchs/telescope-lsp-handlers.nvim'
			use 'jose-elias-alvarez/null-ls.nvim'
			use 'simrat39/symbols-outline.nvim' -- TODO: didn't even realise i had this
			use 'jose-elias-alvarez/typescript.nvim'
			use 'https://git.sr.ht/~whynothugo/lsp_lines.nvim'

			-- dap
			use {
				'mfussenegger/nvim-dap',
				requires = {
					'theHamsta/nvim-dap-virtual-text',
					'rcarriga/nvim-dap-ui',
					{ 'jay-babu/mason-nvim-dap.nvim', requires = { 'williamboman/mason.nvim' } },
				},
			}
			use 'jbyuki/one-small-step-for-vimkind'

			-- completion
			use {
				'hrsh7th/nvim-cmp',
				requires = {
					'hrsh7th/cmp-buffer',
					'hrsh7th/cmp-path',
					'hrsh7th/cmp-cmdline',
					'hrsh7th/cmp-nvim-lsp',
					'hrsh7th/cmp-nvim-lsp-signature-help',
					'onsails/lspkind.nvim',
				},
			}
			use {
				'github/copilot.vim',
				requires = 'hrsh7th/cmp-copilot',
			}
			use {
				'saadparwaiz1/cmp_luasnip',
				requires = { 'L3MON4D3/LuaSnip', 'rafamadriz/friendly-snippets' },
			}

			-- git
			use { 'TimUntersberger/neogit', requires = { 'sindrets/diffview.nvim' } }
			use 'lewis6991/gitsigns.nvim'
			use 'ThePrimeagen/git-worktree.nvim'

			-- help for lua, TODO need to make this work
			-- use 'wsdjeg/luarefvim'
			-- use 'rafcamlet/nvim-luapad'
			use 'milisims/nvim-luaref'
			use 'nanotee/luv-vimdocs'
			use 'folke/neodev.nvim'

			-- langs
			use {
				'LhKipp/nvim-nu',
				run = ':TSInstall nu',
				config = function()
					require('nu').setup {
						complete_cmd_names = true,
					}
				end,
			}

			-- terminal
			use { 'akinsho/toggleterm.nvim', tag = 'v2.*' }
			use 'christoomey/vim-tmux-navigator'

			if packer_bootstrap then packer.sync() end
		end,
		config = {
			max_jobs = 20, -- adapt by machine
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
