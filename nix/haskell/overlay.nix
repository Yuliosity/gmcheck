{ haskell, lib, sources }:

hfinal: hprev:

(lib.listToAttrs (map (a:
  lib.nameValuePair a.name
  (haskell.lib.dontCheck (hfinal.callCabal2nix a.name a.source { }))) [
  ]))
