-- Revert holiplan:schema__app from pg

BEGIN;

  REVOKE USAGE ON SCHEMA app FROM hp_api, hp_auth, hp_user;

  DROP SCHEMA app;

COMMIT;
