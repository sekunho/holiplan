-- Deploy holiplan:schema__auth to pg

BEGIN;

  CREATE SCHEMA auth AUTHORIZATION hp_auth;
  COMMENT ON SCHEMA auth IS
    'Schema that handles authentication';

  GRANT USAGE ON SCHEMA auth TO hp_api, hp_anon, hp_user;
  GRANT USAGE ON SCHEMA app TO hp_auth, hp_api, hp_anon;

COMMIT;
