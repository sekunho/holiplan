-- Revert holiplan:api__users from pg

BEGIN;

  REVOKE EXECUTE ON FUNCTION api.login FROM hp_anon;
  REVOKE EXECUTE ON FUNCTION api.register FROM hp_anon;

  DROP FUNCTION api.login;
  DROP FUNCTION api.register;

COMMIT;
