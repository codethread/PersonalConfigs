#---------------------------------------------#
# EDITOR-ish
# -------------------------------------------#
export alias v = nvim
# see https://github.com/wbthomason/packer.nvim/issues/180 for MACOSX_DEPLOYMENT_TARGET=10.15
export alias nvim-boot = MACOSX_DEPLOYMENT_TARGET=10.15 nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'

# rg --json hi | lines | each {|| from json } | where type == 'match' | get data
export alias vo = ls
export alias als = scope aliases

export use ./git.nu *
