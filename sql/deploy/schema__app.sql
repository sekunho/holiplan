-- Deploy holiplan:schema__app to pg

BEGIN;

  CREATE SCHEMA app;

  GRANT USAGE ON SCHEMA app TO hp_api, hp_auth, hp_user;

COMMIT;
