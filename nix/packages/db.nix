{ pkgs
, migrate
, postgres
}:
with pkgs;
writeShellScriptBin "db" ''
  set -eu
  set -o pipefail

  usage () {
    echo " db [new|copy|backup|clear]"
  }

  clear-db () {
    echo "Stopping postgres"
    ${postgres}/bin/pg_ctl stop
    echo "Clearing db"
    rm -rf "$PGDATA"
  }

  create-db () {
    echo "Creating DB"
    if ! [[ -d $PGDATA ]]; then
      ${postgres}/bin/pg_ctl initdb
      echo "unix_socket_directories = '$PGHOST'" >> "$PGDATA/postgresql.conf"
      echo "logging_collector = on" >> "$PGDATA/postgresql.conf"
      echo "log_statement = all" >> "$PGDATA/postgresql.conf"
      echo "listen_addresses = '''" >> "$PGDATA/postgresql.conf"
    fi

    ${postgres}/bin/pg_ctl stop || echo "Starting pg_ctl"

    ${postgres}/bin/pg_ctl \
      -o "-F -p $PGPORT" \
      start

    PGUSER= PGPASSWORD= ${postgres}/bin/createuser \
      "$PGUSER" \
      --createdb \
      -p $PGPORT || echo "Not creating user"

    PGUSER= PGPASSWORD= ${postgres}/bin/createuser \
      "monitoring" \
      -p $PGPORT || echo "Not creating user"

    PGUSER= PGPASSWORD= ${postgres}/bin/createdb \
      --owner="$PGUSER" \
      "$PGDATABASE" \
      -p $PGPORT || echo "Not creating DB"

    PGUSER= PGPASSWORD= ${postgres}/bin/psql \
      "$PGDATABASE" \
      -c "ALTER ROLE \"$PGUSER\" WITH LOGIN PASSWORD '$PGPASSWORD'" \
      -p $PGPORT || echo "Not adding password"

    export PATH=${postgres}/bin:$PATH
  }

  apply-migrations () {
    echo "Applying migrations"
    SQITCH_USERNAME=$PGUSER ${migrate}/bin/sqitch deploy "db:pg:$PGDATABASE"
  }

  copy-db () {
    TMP_FILE=$(mktemp --tmpdir garnix-dump.sql.XXXXXX)
    ssh root@db.garnix.io "sudo -u garnix pg_dump garnix --port 9178 | bzip2" > $TMP_FILE
    bzcat $TMP_FILE | psql
  }

  backup () {
    FILE="backup-$(date +"%Y-%m-%d-%H:%M").sql"
    ssh root@db.garnix.io "sudo -u garnix pg_dump garnix --port 9178 | bzip2" > $FILE
  }

  new () {
    create-db
    apply-migrations
  }

  copy () {
    clear-db
    create-db
    copy-db
  }

  command=$1; shift

  case "$command" in
    create) create-db ;;
    new) new ;;
    clear) clear-db ;;
    copy) copy ;;
    backup) backup ;;
    *) usage ;;
  esac
''
