-- Deploy holiplan:schema__api to pg

BEGIN;

  CREATE SCHEMA api AUTHORIZATION hp_api;

  COMMENT ON SCHEMA api IS
    'Schema that is meant to be exposed to the API';

  -- Both anonymous and logged in users can interact with the API.
  GRANT USAGE ON SCHEMA api TO hp_anon, hp_user;

COMMIT;
