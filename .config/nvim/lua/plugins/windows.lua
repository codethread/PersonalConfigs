return {
	{ 'xorid/swap-split.nvim', cmd = 'SwapSplit' },

	{ 'shortcuts/no-neck-pain.nvim', version = '*' ,
		cmd={'NoNeckPain'}
	},

	{
		'szw/vim-maximizer',
		cmd = { 'MaximizerToggle' },
		init = function()
			vim.cmd [[
				nnoremap <silent><C-Y> :MaximizerToggle<CR>
				vnoremap <silent><C-Y> :MaximizerToggle<CR>gv
				inoremap <silent><C-Y> <C-o>:MaximizerToggle<CR>
			]]
		end,
	},

	{
		'christoomey/vim-tmux-navigator',
		init = function()
			vim.cmd [[
			" nnoremap <silent> <C-h> :wincmd h<CR>
			" nnoremap <silent> <C-j> :wincmd j<CR>
			" nnoremap <silent> <C-k> :wincmd k<CR>
			" nnoremap <silent> <C-l> :wincmd l<CR>

				" Disable tmux navigator when zooming the Vim pane
				let g:tmux_navigator_disable_when_zoomed = 1
				let g:tmux_navigator_no_mappings = 1
				nnoremap <silent> <C-h> :TmuxNavigateLeft<cr>
				nnoremap <silent> <C-j> :TmuxNavigateDown<cr>
				nnoremap <silent> <C-k> :TmuxNavigateUp<cr>
				nnoremap <silent> <C-l> :TmuxNavigateRight<cr>
			]]
		end,
	},
}
