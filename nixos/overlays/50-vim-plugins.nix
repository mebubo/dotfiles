self: super:

{
  vimPlugins = super.vimPlugins // {
    vim-characterize = self.vimUtils.buildVimPluginFrom2Nix {
      pname = "vim-characterize";
      version = "2019-11-13";
      src = self.fetchFromGitHub {
        owner = "tpope";
        repo = "vim-characterize";
        rev = "af156501e8a8855832f15c2cc3d6cefb2d7f7f52";
        sha256 = "1fmrh94miansi5sz1cwyia7z57azwi4cfxx59h81wrmlsf513l5w";
      };
    };

    vim-ctrlp-tjump = self.vimUtils.buildVimPluginFrom2Nix {
      pname = "vim-ctrlp-tjump";
      version = "2018-03-14";
      src = self.fetchFromGitHub {
    	owner = "ivalkeen";
    	repo = "vim-ctrlp-tjump";
    	rev = "154b5dc7b4651c64a25a4914972b3a5231d5c9d8";
    	sha256 = "1hm2rhls2hn703f78z21wpq5j3s90y5k3qgzi0gq12pnd5cxz358";
      };
    };
  };
}
