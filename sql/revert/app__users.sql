-- Revert holiplan:users from pg

BEGIN;

  DROP TABLE app.users;

  DROP FUNCTION app.current_user_id;
  DROP FUNCTION app.cryptpassword;

COMMIT;
