-- Revert holiplan:holiday_plans from pg

BEGIN;

  REVOKE SELECT, INSERT, UPDATE, DELETE ON TABLE app.comments FROM hp_user;
  REVOKE SELECT, INSERT, UPDATE, DELETE ON TABLE app.events FROM hp_user;
  REVOKE SELECT, INSERT, UPDATE, DELETE ON TABLE app.plans FROM hp_user;

  DROP TABLE app.comments;
  DROP TABLE app.events;
  DROP TABLE app.plans;

COMMIT;
