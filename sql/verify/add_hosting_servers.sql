-- Verify garnix:add_hosting_servers on pg

BEGIN;

SELECT id, configuration_build_id, provider, instance_id, ipv4, ipv6,
       created_at, ended_at, deploy_logs, pull_request, ready_at,
       server_tier, is_primary
  FROM servers WHERE FALSE;

SELECT id, provider, instance_id, created_at, ready_at, ipv4, ipv6, server_tier
  FROM server_pool WHERE FALSE;

ROLLBACK;
