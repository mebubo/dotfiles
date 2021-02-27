self: super: {

  neovim-unwrapped = super.neovim-unwrapped.overrideAttrs (oldAttrs: rec {
	name = "neovim-nightly";
	version = "0.5-nightly";
	src = self.fetchurl {
	  url = "https://github.com/neovim/neovim/commit/8c8cc35926f265bf4f048b83fd130bef3932851e.tar.gz";
	  sha256 = "1rmxvygr9f51s01wvdxn1bkldwhd55kkxj3937dqwaqn8r23zjqv";
	};

	buildInputs = oldAttrs.buildInputs ++ [ self.tree-sitter ];

  });

}
