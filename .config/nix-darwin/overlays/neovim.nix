self: super:

{
  neovim-master = super.neovim.overrideAttrs (old: rec {
    version = "master";
    src = fetchTarball "https://github.com/neovim/neovim/archive/${version}.tar.gz";
  });
}
