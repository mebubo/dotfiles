self: super: {

  neovim-unwrapped = super.neovim-unwrapped.overrideAttrs (oldAttrs: rec {
	name = "neovim-nightly";
	version = "0.5-nightly";
	src = self.fetchurl {
	  url = "https://github.com/neovim/neovim/archive/0a95549d66df63c06d775fcc329f7b63cbb46b2f.tar.gz";
	  sha256 = "0ry564pqjka1h5j5zpv5vpcfqjr47jidajppy90dzgxrl4z2249h";
	};

	buildInputs = oldAttrs.buildInputs ++ [ self.tree-sitter ];

  });

}
