-- Verify garnix:add_hosting_deploy on pg

BEGIN;

SELECT exposed, domains, ssh_users FROM servers WHERE FALSE;

SELECT id, server_id, sampled_at, cpu_pct, mem_used_kb, mem_total_kb
  FROM server_stats WHERE FALSE;

ROLLBACK;
