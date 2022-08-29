-- Deploy holiplan:holiday_plans to pg

BEGIN;

  CREATE TABLE app.plans(
    plan_id     UUID PRIMARY KEY DEFAULT gen_random_uuid(),

    user_id     BIGINT REFERENCES app.users ON DELETE CASCADE,
    name        TEXT NOT NULL,
    description TEXT NOT NULL,
    date        DATE NOT NULL,
    holiday_id  TEXT NOT NULL,
    country     TEXT NOT NULL
  );

  CREATE INDEX plan_user_index ON app.plans(user_id);

  ALTER TABLE app.plans ENABLE ROW LEVEL SECURITY;

  CREATE TABLE app.events(
    event_id   UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    plan_id    UUID REFERENCES app.plans(plan_id) ON DELETE CASCADE,
    user_id    BIGINT REFERENCES app.users ON DELETE CASCADE,

    name       TEXT NOT NULL,
    start_date TIMESTAMP NOT NULL,
    end_date   TIMESTAMP NOT NULL,

    -- Checks if the end_date happens after the start_date.
    CONSTRAINT valid_timestamp CHECK (start_date < end_date),
    CONSTRAINT same_day CHECK (start_date::DATE = end_date::DATE),

    -- Checks if those events with the same `plan_id`s should not have any
    -- overlapping date/time ranges.
    EXCLUDE USING gist (
      plan_id gist_uuid_ops WITH =,
      tsrange(start_date, end_date) WITH &&
    )
  );

  CREATE INDEX event_plan_index ON app.events(plan_id);
  CREATE INDEX event_user_index ON app.events(user_id);

  ALTER TABLE app.events ENABLE ROW LEVEL SECURITY;

  CREATE TABLE app.comments(
    comment_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    plan_id    UUID REFERENCES app.plans,
    user_id    BIGINT REFERENCES app.users,
    content    TEXT NOT NULL
  );

  CREATE INDEX comment_plan_index ON app.comments(plan_id);
  CREATE INDEX comment_user_index ON app.comments(user_id);

  ALTER TABLE app.comments ENABLE ROW LEVEL SECURITY;

  ------------------------------------------------------------------------------
  -- Permissions

  GRANT
      SELECT,
      INSERT,
      UPDATE, DELETE
    ON TABLE app.plans
    TO hp_user;

  CREATE POLICY user_plan_policy
    ON app.plans
    FOR ALL
    USING (user_id = app.current_user_id())
    WITH CHECK (user_id = app.current_user_id());

  GRANT
      SELECT,
      INSERT,
      UPDATE,
      DELETE
    ON TABLE app.events
    TO hp_user;

  CREATE POLICY user_event_policy
    ON app.events
    FOR ALL
    USING (
      user_id = app.current_user_id() AND
      plan_id IN (
        SELECT plan_id
          FROM app.plans
          WHERE user_id = app.current_user_id()
      )
    )
    WITH CHECK (
      user_id = app.current_user_id() AND
      plan_id IN (
        SELECT plan_id
          FROM app.plans
          WHERE user_id = app.current_user_id()
      )
    );

  GRANT
      SELECT,
      INSERT,
      UPDATE, DELETE
    ON TABLE app.comments
    TO hp_user;

  CREATE POLICY user_comment_policy
    ON app.comments
    FOR ALL
    USING (
      user_id = app.current_user_id() AND
      plan_id IN (
        SELECT plan_id
          FROM app.plans
          WHERE user_id = app.current_user_id()
      )
    )
    WITH CHECK
      (
        user_id = app.current_user_id() AND
        plan_id IN (
          SELECT plan_id
            FROM app.plans
            WHERE user_id = app.current_user_id()
        )
      );

COMMIT;
