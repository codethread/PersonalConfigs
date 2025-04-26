local fns = require 'codethread.fns'

-- don't changed jumplist with paragraph jumps :help jumplist
-- nnoremap <silent> } :<C-u>execute "keepjumps norm! " . v:count1 . "}"<CR>
-- nnoremap <silent> { :<C-u>execute "keepjumps norm! " . v:count1 . "{"<CR>

--[[stylua: ignore]] --format
Keys.list({}, {
	{ 'jk'   , 'esc'            , '<ESC>'                                     , mode = 'i' },
	{ 'jj'   , 'backspace word' , '<C-w>'                                     , mode = 'i' },
	{ '<C-d>', 'delete char'    , '<C-o>C'                                    , mode = 'i' },
	{ '}'    , 'next'           , Cmd 'AerialPrev'                             },
	{ '{'    , 'prev'           , Cmd 'AerialNex'                              },
	{ 'ga'   , 'alt file'       , Cmd 'Other'                                  },
	{ 'gx'   , 'Go to link'     , function() require('codethread.gx').gx() end },
	{ 'n'    , 'Center next'    , 'nzzzv'                                      },
	{ 'N'    , 'Center prev'    , 'Nzzzv'                                      },
	{ 'J'    , 'Center join'    , 'mzJ`z'                                      },
	{ 'ZQ'   , 'Quit no save'   , '<cmd>qa!<cr>'                               },
	{ '<C-n>', 'NeoTree'        , Cmd 'Neotree reveal'                         },
	{ '<C-u>', 'Center Up'      , '<C-u>zz'                                    },
	{ '<C-d>', 'Center Up'      , '<C-d>zz'                                    },
	{ '<C-q>', 'Toggle quickfix', fns.toggle_quickfix                          },
})

--[[stylua: ignore]] --format
Keys.list({ mode = 'v' }, {
	{ 'ss', 'live'        , function() require('telescope-live-grep-args.shortcuts').grep_visual_selection() end },
	-- { 'sr', 'find-replace', function() require('spectre').open_visual { select_word = true } end                 },
	{ 'sr', 'find-replace', function () require('grug-far').open({ visualSelectionUsage = 'operate-within-range' }) end },
})

-- overrides marks, these get lost by most formatters making them pretty much useless
-- expose original functionality of marks under 'M'
--[[stylua: ignore]] --format
Keys.list({}, {
	{ 'M' , 'marks'       , 'm'                                                      },
	{ 'ma', 'harpoon.mark', function() require('harpoon.mark').add_file() end        },
	{ 'mf', 'marks'       , Cmd 'Telescope harpoon marks'                            },
	{ 'ml', 'harpoon.ui'  , function() require('harpoon.ui').toggle_quick_menu() end },
	{ 'mk', 'harpoon.ui'  , function() require('harpoon.ui').nav_next() end          },
	{ 'mj', 'harpoon.ui'  , function() require('harpoon.ui').nav_prev() end          },
	{ 'mt', 'harpoon.term', function() require('harpoon.term').gotoTerminal(1) end   },
	{ 'm0', 'harpoon.0'   , function() require('harpoon.ui').nav_file(0) end         },
	{ 'm1', 'harpoon.1'   , function() require('harpoon.ui').nav_file(1) end         },
	{ 'm2', 'harpoon.2'   , function() require('harpoon.ui').nav_file(2) end         },
	{ 'm3', 'harpoon.3'   , function() require('harpoon.ui').nav_file(3) end         },
	{ 'm4', 'harpoon.4'   , function() require('harpoon.ui').nav_file(4) end         },
	{ 'm5', 'harpoon.5'   , function() require('harpoon.ui').nav_file(5) end         },
	{ 'm6', 'harpoon.6'   , function() require('harpoon.ui').nav_file(6) end         },
	{ 'm7', 'harpoon.7'   , function() require('harpoon.ui').nav_file(7) end         },
	{ 'm8', 'harpoon.8'   , function() require('harpoon.ui').nav_file(8) end         },
	{ 'm9', 'harpoon.9'   , function() require('harpoon.ui').nav_file(9) end         },
})

--Fold related
--[[stylua: ignore]] --format
Keys.list({}, {
	{ '-' , 'open fold under cursor'      , 'zc'                                                       },
	{ '=' , 'close fold under cursor'     , 'zo'                                                       },
	{ '_' , 'close all folds under cursor', 'zC'                                                       },
	{ '+' , 'open all folds under cursor' , 'zO'                                                       },
	{ 'zR', 'open all folds'              , function() require('ufo').openAllFolds() end               },
	{ 'zM', 'close all folds'             , function() require('ufo').closeAllFolds() end              },
	{ 'zp', 'peak fold'                   , function() require('ufo').peekFoldedLinesUnderCursor() end },
	{ 'zc', 'open fold under cursor'      , function() require 'ufo' end                               },
	{ 'zs', 'set fold level'              , require('codethread.fold').set_fold_level                  },
})
