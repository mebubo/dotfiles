self: super: {

  neovim-unwrapped = super.neovim-unwrapped.overrideAttrs (oldAttrs: rec {
	name = "neovim-nightly";
	version = "0.5-nightly-2021-06-20";
	src = self.fetchFromGitHub {
	  owner = "neovim";
	  repo = "neovim";
	  rev = "997a9c879215bc01a928de3e762955878314ec6a";
	  sha256 = "1jli9z1yycmzmfpzj7f3yw61ddsral8nnnffrljzzyjn817ix79n";
	};

	buildInputs = oldAttrs.buildInputs ++ [ self.tree-sitter ];

  });

}
