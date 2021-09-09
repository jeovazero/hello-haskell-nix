{ nixpkgs ? import ./nix/pinnedNix.nix { } }:
let
  inherit (nixpkgs) pkgs;
  pg = pkgs.postgresql_12;
  sqitch = pkgs.sqitchPg;
  haskellPkgs = pkgs.haskell.packages.ghc8104;

  hello = haskellPkgs.callCabal2nix "hello" ./. { };
  envs = ''
    export SOCKET_DIR=/tmp/socket_dir_hello_db

    export PGDATABASE=dev
    export PGUSER=dev
    export PGPASSWORD=pass
    export PGPORT=5444
    export PGDATA=$SOCKET_DIR/pg
    export PGSOCK=$SOCKET_DIR/.s.PGSQL.$PGPORT

    export TPG_DB=$PGDATABASE
    export TPG_USER=$PGUSER
    export TPG_PASS=$PGPASSWORD
    export TPG_PORT=$PGPORT
    export TPG_SOCK=$PGSOCK
    #export TPG_DEBUG=true

  '';

  migrations = pkgs.stdenv.mkDerivation {
    name = "migrations";
    buildInputs = [];
    src = ./migrations;
    installPhase = ''
      mkdir -p $out/migrations
      cp . $out/migrations -r
    '';
   };
in
  pkgs.haskell.lib.overrideCabal hello (old: {
    buildDepends = (old.buildDepends or []) ++ [pg migrations sqitch];
    preBuild = ''
      ${old.preBuild or ""}
      ${envs}
      mkdir $SOCKET_DIR
      echo debuh=$TPG_DEBUG
      echo "PREBVUILD"
      echo debuh=$TPG_DEBUG
      echo "Init DB"
      initdb $PGDATA
        
      echo "Start DB"
      postgres -D $PGDATA -k $SOCKET_DIR  &

      echo "Waiting..."
      while [[ ! -e $PGSOCK ]]; do
        echo "wait"
        sleep 0.1;
      done

      sleep 1
      echo "Create user"
      createuser -h $SOCKET_DIR -U $(whoami) -s $PGUSER

      echo "Create db"
      createdb -h $SOCKET_DIR -O $PGUSER $PGDATABASE

      echo "DB Created"

      sleep 1
      echo "apply migrations"
      cd ${migrations}/migrations
      ls
      sqitch deploy

      # very important
      cd -
    '';
    postInstall = ''
      ${old.postInstall or ""}
      ${envs}
      echo "Kill Postgres"
      pg_ctl stop || true

      echo "Waiting"
      while [[ -e $PGSOCK ]]; do
        echo "wait"
        sleep 0.1;
      done

      echo "Delete socket dir"
      rm -rf $SOCKET_DIR
    '';
  })
