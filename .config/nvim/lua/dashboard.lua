local alpha = require("alpha")
local dashboard = require("alpha.themes.dashboard")

local simple = {
	[[                                                    ]],
	[[                                                    ]],
	[[                                                    ]],
	[[                                                    ]],
	[[                                                    ]],
	[[                                  __                ]],
	[[     ___     ___    ___   __  __ /\_\    ___ ___    ]],
	[[    / _ `\  / __`\ / __`\/\ \/\ \\/\ \  / __` __`\  ]],
	[[   /\ \/\ \/\  __//\ \_\ \ \ \_/ |\ \ \/\ \/\ \/\ \ ]],
	[[   \ \_\ \_\ \____\ \____/\ \___/  \ \_\ \_\ \_\ \_\]],
	[[    \/_/\/_/\/____/\/___/  \/__/    \/_/\/_/\/_/\/_/]],
	[[                                                    ]],
	[[                                                    ]],
	[[                                                    ]],
}

dashboard.section.header.val = simple

dashboard.section.buttons.val = {
	dashboard.button("e", "  New File    ", ":enew<CR>"),
	dashboard.button("f", "  Find File   ", ":Telescope find_files<CR>"),
	dashboard.button("t", "  Find Text   ", ":Telescope live_grep<CR>"),
	dashboard.button("c", "  NVIM Config ", ":Telescope dotfiles<CR>"),
	dashboard.button("q", "  Quit        ", ":qa<CR>"),
}

dashboard.section.footer.val = require("alpha.fortune")()

alpha.setup(dashboard.opts)
