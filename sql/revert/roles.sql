-- Revert holiplan:roles from pg

BEGIN;

REVOKE hp_anon, hp_user FROM hp_authenticator;

ALTER DEFAULT PRIVILEGES GRANT EXECUTE ON functions TO public;
ALTER DEFAULT PRIVILEGES FOR ROLE hp_auth, hp_api GRANT EXECUTE ON functions TO public;

DROP ROLE hp_authenticator;
DROP ROLE hp_anon;
DROP ROLE hp_user;
DROP ROLE hp_auth;
DROP ROLE hp_api;

COMMIT;
