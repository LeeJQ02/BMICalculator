{ pkgs }: {
    deps = [
        pkgs.ihaskell
        (pkgs.haskellPackages.ghcWithPackages (pkgs: [
            # Put your dependencies here!
        ]))
        pkgs.haskell-language-server
    ];
}