-- Revert holiplan:auth__sessions from pg

BEGIN;
  REVOKE EXECUTE ON FUNCTION auth.login FROM hp_anon, hp_api;
  REVOKE EXECUTE ON FUNCTION auth.refresh_session FROM hp_user;
  REVOKE EXECUTE ON FUNCTION auth.logout FROM hp_user;
  REVOKE EXECUTE ON FUNCTION auth.get_user_session FROM hp_anon;
  REVOKE EXECUTE ON FUNCTION auth.authenticate FROM hp_anon;

  DROP FUNCTION auth.clean_sessions;
  DROP FUNCTION auth.login;
  DROP FUNCTION auth.refresh_session;
  DROP FUNCTION auth.logout;
  DROP FUNCTION auth.authenticate;
  DROP FUNCTION auth.get_user_session;
  DROP VIEW auth.active_sessions;
  DROP TABLE auth.sessions;
COMMIT;
