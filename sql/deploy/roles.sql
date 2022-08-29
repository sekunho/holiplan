-- Deploy holiplan:roles to pg

BEGIN;

  ------------------------------------------------------------------------------
  -- Roles
  --
  -- holiplan has the following roles:
  -- 1. `hp_authenticator`
  -- 2. `hp_anon`
  -- 3. `hp_auth`
  -- 4. `hp_api`
  -- 5. `hp_user`

  CREATE ROLE hp_authenticator NOINHERIT LOGIN;
  COMMENT ON ROLE hp_authenticator IS
    'Role that servers as an entrypoint';

  CREATE ROLE hp_anon NOINHERIT NOLOGIN;
  COMMENT ON ROLE hp_anon IS
    'Role that will be switched to when a user is not authenticated';

  CREATE ROLE hp_auth NOLOGIN;
  COMMENT ON ROLE hp_auth IS
    'Role that owns the `auth` schema and its objects';

  CREATE ROLE hp_api NOLOGIN;
  COMMENT ON ROLE hp_api IS
    'Role that owns the `api` schema and its objects';

  CREATE ROLE hp_user NOINHERIT LOGIN;
  COMMENT ON ROLE hp_user IS
    'Role that will be switched to when a user is authenticated';

  ------------------------------------------------------------------------------
  -- Permissions

  -- Allows `hp_authenticator` to switch to any of the ff:
  -- 1. `hp_anon`
  -- 2. `hp_user`

  GRANT hp_anon, hp_user TO hp_authenticator;

  -- Remove default privileges to execute functions.
  ALTER DEFAULT PRIVILEGES REVOKE EXECUTE ON functions FROM public;

  -- Remove privileges for executing functions for `hp_auth` and `hp_api`.
  ALTER
    DEFAULT PRIVILEGES FOR ROLE hp_auth, hp_api
    REVOKE EXECUTE ON functions FROM public;

COMMIT;
