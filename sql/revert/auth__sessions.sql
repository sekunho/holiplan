-- Revert holiplan:auth__sessions from pg

BEGIN;
  REVOKE EXECUTE ON FUNCTION auth.login FROM hp_anon, hp_api;
  REVOKE EXECUTE ON FUNCTION auth.authenticate FROM hp_anon;

  DROP FUNCTION auth.login;
  DROP FUNCTION auth.authenticate;

COMMIT;
