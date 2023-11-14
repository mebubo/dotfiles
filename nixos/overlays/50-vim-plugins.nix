self: super:

{
  vimPlugins = super.vimPlugins // {
    vim-characterize = self.vimUtils.buildVimPlugin {
      pname = "vim-characterize";
      version = "2019-11-13";
      src = self.fetchFromGitHub {
        owner = "tpope";
        repo = "vim-characterize";
        rev = "af156501e8a8855832f15c2cc3d6cefb2d7f7f52";
        sha256 = "1fmrh94miansi5sz1cwyia7z57azwi4cfxx59h81wrmlsf513l5w";
      };
    };

    vim-ctrlp-tjump = self.vimUtils.buildVimPlugin {
      pname = "vim-ctrlp-tjump";
      version = "2018-03-14";
      src = self.fetchFromGitHub {
    	owner = "ivalkeen";
    	repo = "vim-ctrlp-tjump";
    	rev = "154b5dc7b4651c64a25a4914972b3a5231d5c9d8";
    	sha256 = "1hm2rhls2hn703f78z21wpq5j3s90y5k3qgzi0gq12pnd5cxz358";
      };
    };

    nvim-metals = self.vimUtils.buildVimPlugin {
      pname = "nvim-metals";
      version = "2021-07-18";
      src = self.fetchFromGitHub {
        owner = "scalameta";
        repo = "nvim-metals";
        rev = "4c9ed42f7070aa25eee4241f146b1f5b5adee93d";
        sha256 = "1jbc9pbhjfzx8h5m652dwaximqvsh934rki5dkma6lpmy9srq7b8";
      };
    };

  };
}
