{ ... }:
{
  name = "test fluent bit module";

  nodes = {
    client =
      {
        config,
        lib,
        nodes,
        pkgs,
        ...
      }:
      {
        garnix.monitoring-client.nginx.enable = false;
        garnix.fluent-bit = {
          enable = lib.mkForce true;
          enableNginxLogParsing = true;
          devModeOutputsToFile = false;
          extraGroups = [ "nginx" ];
          configuration = {
            parsers = {
              app = {
                name = "app";
                format = "json";
                time_key = "time";
                time_format = "%Y-%m-%d %H:%M:%S";
              };
            };
            pipelines = {
              app = {
                input = {
                  Name = "http";
                  Port = 8888;
                  Tag = "app";
                };
                filter = {
                  Name = "grep";
                  Match = "app";
                  Exclude = "category info";
                };
                output = {
                  Name = "stdout";
                  Format = "json";
                  Match = "app";
                };
              };
              # disable the journal pipeline (tested in opensearch.nix)
              journal.enable = false;
              nginx.output = lib.mkForce {
                Name = "stdout";
                Format = "json";
                Match = "nginx";
              };
            };
          };
        };
        services.nginx = {
          enable = true;
          virtualHosts."default" = {
            locations."/" = {
              root = pkgs.runCommand "testdir" { } ''
                mkdir "$out"
                echo hello world > "$out/hello.html"
              '';
            };
          };
        };
        virtualisation.memorySize = 2048;
      };
  };

  testScript = { nodes, ... }: ''
    import json
    start_all()
    client.wait_for_unit("multi-user.target")
    client.wait_for_unit("fluent-bit.service")

    with subtest("Test app logs through http input"):
      client.wait_for_open_port(8888)
      client.succeed('curl -d \'{"msg":"Started !", "time": "2024-01-02 11:12:05", "category":"info"}\' -XPOST -H "content-type: application/json" http://localhost:8888/app')
      client.succeed('curl -d \'{"msg":"Hello world !", "time": "2024-01-02 11:12:06", "category":"important"}\' -XPOST -H "content-type: application/json" http://localhost:8888/app')
      client.wait_until_succeeds("journalctl -u fluent-bit.service | grep 'Hello world !'")
      client.fail("journalctl -u fluent-bit.service | grep 'Started !'")

    with subtest("Test nginx logs through file input"):
      client.succeed("curl http://localhost/hello.html")
      output = client.wait_until_succeeds("journalctl -u fluent-bit.service -n 1 -g 'GET /hello.html' --output cat | jq .[]")
      json_output = json.loads(output)
      assert json_output['request_method'] == "GET"
      assert json_output['status'] == "200"
      assert json_output['request_uri'] == "/hello.html"
      assert json_output['server'] == "client"
      assert json_output['bytes_sent'] > 0
  '';
}
