self: super: {

  neovim-unwrapped = super.neovim-unwrapped.overrideAttrs (oldAttrs: rec {
	name = "neovim-nightly";
	version = "0.5-nightly-2021-02-27";
	src = self.fetchurl {
	  url = "https://github.com/neovim/neovim/archive/c1fbc2ddf15b2f44b615f90b2511349ab974cb83.tar.gz";
	  sha256 = "09sbvv124mnj1s21nd5q5y0z11r13h0n9yak0dgp6dpqf9lxa7lh";
	};

	buildInputs = oldAttrs.buildInputs ++ [ self.tree-sitter ];

  });

}
