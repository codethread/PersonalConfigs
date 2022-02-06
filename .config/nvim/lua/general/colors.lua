vim.g.nord_italic = true
vim.g.nord_borders = true
vim.g.nord_contrast = true

require('nord').set()

require('lualine').setup {
  options = {
    theme = 'nord'
  }
}
