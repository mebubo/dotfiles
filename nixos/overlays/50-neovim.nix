self: super: {

  neovim-unwrapped = super.neovim-unwrapped.overrideAttrs (oldAttrs: rec {
	name = "neovim-nightly";
	version = "0.5-nightly-2021-02-27";
	src = self.fetchFromGitHub {
	  owner = "neovim";
	  repo = "neovim";
	  rev = "a129887c00a2d5e49fc551ba0bbffe88cefb56c0";
	  sha256 = "0ixlbpmc5kmbwbj1aj41ip5l122dzcmq1rzxfrxy9vkmy4madqpz";
	};

	buildInputs = oldAttrs.buildInputs ++ [ self.tree-sitter ];

  });

}
