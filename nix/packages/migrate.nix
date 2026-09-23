# A wrapper with a configured sqitch
{ pkgs }:
pkgs.symlinkJoin {
  name = "migrate";
  buildInputs = [ pkgs.makeWrapper ];
  paths = [
    pkgs.sqitchPg
    ../../sql
  ];
  postBuild = ''
    wrapProgram "$out/bin/sqitch" \
      --run "cd $out" \
      --add-flags "--client=${pkgs.postgresql_18}/bin/psql" \
      --set SQITCH_CONFIG $out/sqitch.conf \
      --set LC_NUMERIC="C.UTF8"
  '';
}
