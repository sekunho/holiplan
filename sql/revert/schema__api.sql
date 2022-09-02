-- Revert holiplan:schema__api from pg

BEGIN;
  REVOKE USAGE ON SCHEMA api FROM hp_anon, hp_user;

  DROP SCHEMA api;

COMMIT;
