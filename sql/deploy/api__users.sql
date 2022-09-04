-- Deploy holiplan:api__users to pg

BEGIN;
  SET LOCAL ROLE hp_api;

  CREATE FUNCTION api.login(username TEXT, password TEXT)
    RETURNS JSONB
    LANGUAGE SQL
    AS $$
      SELECT *
        FROM auth.login(login.username :: CITEXT, login.password);
    $$;

  COMMENT ON FUNCTION api.login IS
    'Logs a user in';

  GRANT EXECUTE ON FUNCTION api.login TO hp_anon;

  CREATE FUNCTION api.register(username TEXT, password TEXT)
    RETURNS JSONB
    SECURITY DEFINER
    LANGUAGE SQL
    AS $$
      INSERT
        INTO app.users(username, password)
        VALUES (register.username :: CITEXT, register.password)
        RETURNING json_build_object('user_id', user_id);
    $$;

    COMMENT ON FUNCTION api.register IS
      'Registers a new user';

    GRANT EXECUTE ON FUNCTION api.register TO hp_anon;
COMMIT;
