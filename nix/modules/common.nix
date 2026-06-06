{ config, lib, pkgs, flakeInputs, ... }:

{
  options = {
    garnix = {
      useGarnixCache = lib.mkOption {
        type = lib.types.bool;
        default = true;
      };
    };
  };

  config = {
    environment.systemPackages = with pkgs; [
      bat
      binutils
      bottom
      choose
      # coreutils is not available by default on darwin
      coreutils
      dig
      dust
      fd
      file
      git
      hexyl
      htop
      iftop
      jq
      lsof
      openssl
      parallel
      pstree
      renameutils
      ripgrep
      screen
      sd
      silver-searcher
      tcpdump
      tmux
      tree
      vim
      zellij
      flakeInputs.treetop.packages.${stdenv.hostPlatform.system}.default
    ];

    nix = {
      settings = {
        experimental-features = [
          "nix-command"
          "flakes"
          "recursive-nix"
        ];
        substituters = lib.mkIf config.garnix.useGarnixCache [
          "https://cache.garnix.io"
        ];
        trusted-public-keys = lib.mkIf config.garnix.useGarnixCache [
          "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
        ];
        builders-use-substitutes = true;
        use-registries = false;
        max-silent-time = "3600";
        timeout = "7200";
        connect-timeout = "10";
        fallback = true;
        narinfo-cache-negative-ttl = 60;
        narinfo-cache-positive-ttl = 120;
        download-buffer-size = 640 * 1024 * 1024;
      };

      gc = {
        automatic = true;
        options = "--delete-older-than 30d --keep-going";
      };
    };

  };
}
