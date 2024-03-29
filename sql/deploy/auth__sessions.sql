-- Deploy holiplan:auth__sessions to pg

BEGIN;

SET LOCAL ROLE hp_auth;

--------------------------------------------------------------------------------

CREATE TABLE auth.sessions (
  token TEXT NOT NULL PRIMARY KEY
        DEFAULT encode(gen_random_bytes(32), 'base64'),
  user_id INTEGER REFERENCES app.users (user_id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  expires_on TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp() + '30 days' :: INTERVAL,

  CHECK (expires_on > created_at)
);

COMMENT ON TABLE auth.sessions IS
  'User sessions';

COMMENT ON COLUMN auth.sessions.expires_on IS
  'Time on which the session expires';

CREATE INDEX ON auth.sessions (expires_on);

--------------------------------------------------------------------------------

CREATE VIEW auth.active_sessions AS
  SELECT
      token,
      user_id,
      created_at,
      expires_on
    FROM auth.sessions
    WHERE expires_on > clock_timestamp()
    WITH LOCAL CHECK OPTION;

COMMENT ON VIEW auth.active_sessions IS
  'A view for the sessions that are currently active';

--------------------------------------------------------------------------------
-- Functions

-- TODO: pgcron or some cron job
CREATE FUNCTION auth.clean_sessions()
  RETURNS VOID
  LANGUAGE sql
  SECURITY DEFINER
  AS $$
    DELETE FROM auth.sessions
      WHERE expires_on < clock_timestamp() - '1day' :: INTERVAL;
  $$;

COMMENT ON FUNCTION auth.clean_sessions IS
  'Cleans up sessions that have expired longer than a day ago';

--------------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION auth.login(username CITEXT, password TEXT)
  RETURNS JSONB
  LANGUAGE sql
  SECURITY DEFINER
  AS $$
    INSERT INTO auth.active_sessions(user_id)
      SELECT user_id
        FROM app.users
        WHERE username = login.username
          AND password = crypt(login.password, password)
        RETURNING jsonb_build_object
          ( 'token'
          , token
          , 'expires_on'
          , expires_on
          , 'user_id'
          , user_id
          );
  $$;

COMMENT ON FUNCTION auth.login IS
  'Returns the token for a newly created session, or NULL on failure';

GRANT EXECUTE ON FUNCTION auth.login TO hp_anon, hp_api;

--------------------------------------------------------------------------------

CREATE FUNCTION auth.refresh_session(session_token TEXT)
  RETURNS VOID
  LANGUAGE sql
  SECURITY DEFINER
  AS $$
    UPDATE auth.sessions
    SET expires_on = DEFAULT
    WHERE token = session_token AND expires_on > clock_timestamp();
  $$;

COMMENT ON FUNCTION auth.refresh_session IS
  'Extend the expiration time of the given session';

GRANT EXECUTE ON FUNCTION auth.refresh_session TO hp_user;

--------------------------------------------------------------------------------

CREATE FUNCTION auth.logout(session_token TEXT)
  RETURNS VOID
  LANGUAGE sql
  SECURITY DEFINER
  AS $$
    UPDATE auth.sessions
    SET expires_on = clock_timestamp()
    WHERE token = session_token;
  $$;

GRANT EXECUTE ON FUNCTION auth.logout TO hp_user;

--------------------------------------------------------------------------------

CREATE FUNCTION auth.get_user_session(session_token TEXT)
  RETURNS JSONB
  LANGUAGE sql
  SECURITY DEFINER
  AS $$
    SELECT jsonb_build_object
            ( 'user_id'
            , user_id
            , 'token'
            , session_token
            , 'expires_on'
            , expires_on
            -- , 'created_at'
            -- , created_at
            )
      FROM auth.active_sessions
      WHERE token = session_token;
  $$;

GRANT EXECUTE ON FUNCTION auth.get_user_session TO hp_anon;

--------------------------------------------------------------------------------

CREATE FUNCTION auth.authenticate()
  RETURNS VOID
  LANGUAGE plpgsql
  AS $$
    DECLARE
      session_token   TEXT;
      session_user_id INT;
    BEGIN
      -- SELECT current_setting('request.session_token', TRUE)
      --   INTO session_token;

      SELECT current_setting('request.session_user_id', TRUE)
        INTO session_user_id;

      -- SELECT auth.session_user_id(session_token)
      --   INTO session_user_id;

      IF session_user_id IS NOT NULL THEN
        SET LOCAL ROLE hp_user;

        PERFORM set_config('auth.user_id', session_user_id :: TEXT, TRUE);
      ELSE
        SET LOCAL ROLE hp_anon;
      END IF;
    END;
  $$;

COMMENT ON FUNCTION auth.authenticate IS
  'Sets the role and user ID for the current transaction based on the cookie';

GRANT EXECUTE ON FUNCTION auth.authenticate TO hp_anon;

--------------------------------------------------------------------------------

COMMIT;
