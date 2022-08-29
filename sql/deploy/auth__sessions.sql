-- Deploy holiplan:auth__sessions to pg

BEGIN;
  SET LOCAL ROLE hp_auth;

  CREATE FUNCTION auth.login(username CITEXT, password TEXT)
    RETURNS JSONB
    LANGUAGE SQL
    SECURITY DEFINER
    AS $$
      SELECT json_build_object('user_id', users.user_id)
        FROM app.users
        WHERE username = login.username
          AND password = crypt(login.password, password)
    $$;

  GRANT EXECUTE ON FUNCTION auth.login TO hp_anon, hp_api;

  CREATE OR REPLACE FUNCTION auth.authenticate()
    RETURNS VOID
    LANGUAGE PLPGSQL
    AS $$
      DECLARE
        session_user_id BIGINT;
        session_role TEXT;
      BEGIN
        -- If there's a user ID in claims, then it's assumed that the person
        -- is logged in. So, we set the role (only for the lifetime of the
        -- transaction) as `hp_user`. Otherwise if it doesn't exist, then it is
        -- assumed to be an anonymous user.
        SELECT current_setting('claims.user_id', true)
          INTO session_user_id;

        IF session_user_id IS NOT NULL THEN
          SET LOCAL ROLE hp_user;

          PERFORM set_config('auth.user_id', session_user_id :: TEXT, true);
        ELSE
          SET LOCAL ROLE hp_anon;

          PERFORM set_config('auth.user_id', '', true);
        END IF;
      END;
    $$;

    COMMENT ON FUNCTION auth.authenticate IS
      'Sets the role as LOCAL, and the user_id in current_setting.';

    GRANT EXECUTE ON FUNCTION auth.authenticate TO hp_anon;

COMMIT;
