{
  stdenv, fetchFromGitHub, rustPlatform
, libudev, pkgconfig
}:

let
  pname = "kanshi";
  version = "ed21acce0c52f7893c903f46b09b4a3b55e2c198";
  sha256 = "1syvws61m8xj81kg7r0gahf6p829pmjry84hp80c5j6l11imi2hb";
in
rustPlatform.buildRustPackage {
  inherit pname version;
  name = "${pname}-${version}";

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ libudev ];

  cargoBuildFlags = [];

  src = fetchFromGitHub {
    owner = "emersion";
    repo = pname;
    rev = version;
    inherit sha256;
  };

  cargoSha256Version = 2;
  cargoSha256 = "098q1g04d5mpwlw1gshm78x28ki4gwhlkwqsd8vrfhp96v97n1sf";

  meta = with stdenv.lib; {
    description = "Dynamic display configuration";
    homepage = "https://github.com/emersion/kanshi";
    maintainers = with maintainers; [ colemickens ];
    platforms = platforms.linux;
    license = licenses.mit;
  };
}
