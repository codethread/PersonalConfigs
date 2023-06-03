return {

	{
		'famiu/bufdelete.nvim',
		cmd = 'Bdelete',
	},

	{
		'mbbill/undotree',
		init = function()
			vim.cmd [[
				" add undo break points on key stroke to make undo more granular
				inoremap , ,<c-g>u
				inoremap . .<c-g>u
				inoremap ( (<c-g>u
				inoremap { {<c-g>u
			]]

			vim.opt.swapfile = false
			vim.opt.backup = false
			vim.opt.writebackup = false -- This is recommended by coc

			vim.cmd [[
        if has("persistent_undo")
           let target_path = expand('~/.local/share/nvim/undodir')

            " create the directory and any parent directories
            " if the location does not exist.
            if !isdirectory(target_path)
                call mkdir(target_path, "p", 0700)
            endif

            let &undodir=target_path
            set undofile
        endif
        ]]
		end,
	},

	{ 'wakatime/vim-wakatime', event = { 'BufReadPre', 'BufNewFile' }, version = '9.*' },

	{
		'AndrewRadev/bufferize.vim',
		cmd = 'Bufferize',
		keys = {
			{ '<leader>Gm', Cmd 'Bufferize messages', 'messages' },
		},
	},
}
