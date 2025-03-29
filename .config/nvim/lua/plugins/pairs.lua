return U.F {
	{ 'echasnovski/mini.surround', opts = {} },

	--use i_ctrl_v for literal inserts
	{
		'echasnovski/mini.pairs',
		opts = {},
		enabled = false, -- just trying to type instead
		init = function()
			U.au(
				'Toggle autopairs in macros',
				{ 'RecordingEnter', 'RecordingLeave' },
				function(e) vim.b[e.buf].minipairs_disable = e.event == 'RecordingEnter' end
			)
		end,
	},
	-- {
	-- 	'windwp/nvim-autopairs',
	-- 	event = 'InsertEnter',
	-- 	config = true,
	-- },

	-- close <div tags, and ciw
	{ 'windwp/nvim-ts-autotag', opts = {}, event = { 'InsertEnter' } },
}
