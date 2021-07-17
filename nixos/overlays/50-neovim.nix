self: super: {

  neovim-unwrapped = super.neovim-unwrapped.overrideAttrs (oldAttrs: rec {
	name = "neovim";
	version = "0.5.0";
	src = self.fetchFromGitHub {
	  owner = "neovim";
	  repo = "neovim";
	  rev = "v0.5.0";
	  sha256 = "0lgbf90sbachdag1zm9pmnlbn35964l3khs27qy4462qzpqyi9fi";
	};

	buildInputs = oldAttrs.buildInputs ++ [ self.tree-sitter ];

  });

}
