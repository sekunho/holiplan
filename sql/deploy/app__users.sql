-- Deploy holiplan:users to pg

BEGIN;

  ------------------------------------------------------------------------------
  -- Functions

  CREATE FUNCTION app.current_user_id()
    RETURNS BIGINT
    LANGUAGE SQL
    AS $$
      SELECT NULLIF(current_setting('auth.user_id', true), '') :: BIGINT
    $$;

  COMMENT ON FUNCTION app.current_user_id() IS
    'Gets the current user ID from the `current_setting`';

  GRANT EXECUTE ON FUNCTION app.current_user_id TO hp_api, hp_user;

  -- Encrypt password
  -- This is triggered before every insert or update in `app.users`.
  CREATE FUNCTION app.cryptpassword()
    RETURNS TRIGGER
    LANGUAGE PLPGSQL
    AS $$
      BEGIN
        IF tg_op = 'INSERT' OR NEW.password <> OLD.password THEN
          NEW.password = crypt(new.password, gen_salt('bf'));
        END IF;

        RETURN NEW;
      END;
    $$;

  ------------------------------------------------------------------------------
  -- Tables

  CREATE TABLE app.users(
    user_id  BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,

    -- TODO: Validate
    username CITEXT NOT NULL,
    password TEXT NOT NULL,

    CONSTRAINT password_length
      CHECK (char_length(password) <= 72 AND char_length(password) >= 10)
  );

  ------------------------------------------------------------------------------
  -- Permissions of `app.users`

  -- `hp_api` permissions
  GRANT
      SELECT (user_id, username),
      INSERT (username, password),
      UPDATE (username, password)
    ON TABLE app.users
    TO hp_api;

  -- `hp_auth` permissions
  -- Auth should be able to select the necessary information to perform a login.
  GRANT
      REFERENCES,
      SELECT (user_id, username, password)
    ON TABLE app.users
    TO hp_auth;

  ALTER TABLE app.users ENABLE ROW LEVEL SECURITY;

  CREATE POLICY user_read_profile
    ON app.users
    FOR SELECT
    USING (user_id = app.current_user_id());

  -- Users can update their own profile if they wish.
  -- This checks if the user has a role of `hp_user`, and if the user ID matches
  -- the user ID in the transaction.
  CREATE POLICY user_update_profile
    ON app.users
    FOR UPDATE
    USING (
      current_setting('auth.role') = 'hp_user' AND
      user_id = app.current_user_id()
    )
    WITH CHECK (user_id = app.current_user_id());

  -- `hp_auth` has permissions to SELECT rows from `app.users` so it can validate
  -- whatever is needed to perform a login.
  CREATE POLICY auth_read_users
    ON app.users
    FOR SELECT
    TO hp_auth
    USING (true);

  CREATE POLICY api_reads_user_id
    ON app.users
    FOR SELECT
    TO hp_api
    USING (true);

  -- `hp_api` has permissions to register users.
  CREATE POLICY api_register_users
    ON app.users
    FOR INSERT
    TO hp_api
    WITH CHECK (true);

  CREATE TRIGGER cryptpassword
    BEFORE INSERT OR UPDATE
    ON app.users
    FOR EACH ROW
      EXECUTE PROCEDURE app.cryptpassword();

COMMIT;
