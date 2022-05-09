with import <nixpkgs> {};
stdenv.mkDerivation {
    name = "diving-beet";
    buildInputs = [ pkgconfig SDL2 SDL2_ttf SDL2_mixer SDL2_gfx go ocl-icd opencl-headers ];
}
