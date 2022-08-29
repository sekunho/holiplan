-- Revert holiplan:schema__auth from pg

BEGIN;
  REVOKE USAGE ON SCHEMA auth FROM hp_api, hp_anon, hp_user;
  REVOKE USAGE ON SCHEMA app FROM hp_auth, hp_api, hp_anon;

  DROP SCHEMA auth;

COMMIT;
