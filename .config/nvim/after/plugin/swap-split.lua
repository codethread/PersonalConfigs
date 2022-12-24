local safe_load = require('codethread.utils').safe_load

safe_load('swap-split', function(swapsplit)
	swapsplit.setup {
		ignore_filetypes = {
			'NvimTree',
		},
	}

	vim.cmd [[
        hi! link SwapSplitStatusLine CursorLine
    ]]
end)
