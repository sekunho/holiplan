-- Deploy holiplan:api__holiday_plans to pg

BEGIN;

  SET LOCAL ROLE hp_api;

  --------------------------------------------------------------------------------
  -- Plan-related functions

  CREATE OR REPLACE FUNCTION api.create_plan
    ( in_name        TEXT
    , in_description TEXT
    , in_plan_date   DATE
    , in_holiday_id  TEXT
    , in_country     TEXT
    )
    RETURNS JSONB
    LANGUAGE SQL
    AS $$
      INSERT
        INTO app.plans (name, description, date, user_id, holiday_id, country)
        VALUES
          ( $1
          , $2
          , $3
          , app.current_user_id()
          , in_holiday_id
          , in_country
          )
        RETURNING
          json_build_object
            ( 'id'
            , id
            , 'name'
            , name
            , 'description'
            , description
            , 'date'
            , date
            , 'user_id'
            , user_id
            , 'holiday_id'
            , holiday_id
            , 'country'
            , country
            );
    $$;

  GRANT EXECUTE ON FUNCTION api.create_plan TO hp_user;

  CREATE OR REPLACE FUNCTION api.get_plan_details(in_plan_id UUID)
    RETURNS JSONB
    LANGUAGE PLPGSQL
    AS $$
      DECLARE
        result JSONB;
      BEGIN
        WITH events_cte AS (
          SELECT jsonb_agg(events) AS events
            FROM app.events
            WHERE events.plan_id = in_plan_id
        ), comments_cte AS (
          SELECT jsonb_agg(comments) AS comments
            FROM app.comments
            WHERE comments.plan_id = in_plan_id
        )
        SELECT
          json_build_object
            ( 'name'
            , plans.name
            , 'description'
            , plans.description
            , 'id'
            , plans.id
            , 'events'
            , coalesce(events_cte.events, '[]'::JSONB)
            , 'comments'
            , coalesce(comments_cte.comments, '[]'::JSONB)
            , 'country'
            , plans.country
            , 'holiday_id'
            , plans.holiday_id
            , 'date'
            , plans.date
            )
          INTO result
          FROM app.plans, events_cte, comments_cte
          WHERE plans.id = in_plan_id
          GROUP BY
            plans.id
          , plans.name
          , plans.description
          , events_cte.events
          , comments_cte.comments;

        IF result IS NULL THEN
          RAISE
            'Resource does not exist: %',
            in_plan_id USING ERRCODE = 'no_data_found';
        END IF;

        RETURN result;
      END;
    $$;

  GRANT EXECUTE ON FUNCTION api.get_plan_details TO hp_user;

  -- TODO: Add user ID in argument
  CREATE OR REPLACE FUNCTION api.list_plans()
    RETURNS JSONB
    LANGUAGE SQL
    AS $$
      SELECT
        json_build_object
          ( 'plans'
          , coalesce(jsonb_agg(row_to_json(plans)), '[]'::JSONB)
          , 'length'
          , count(plans)
          )
        FROM app.plans;
    $$;

  GRANT EXECUTE ON FUNCTION api.list_plans TO hp_user;

  CREATE OR REPLACE FUNCTION api.edit_plan
    ( in_plan_id UUID
    , in_name TEXT
    , in_description TEXT
    )
    RETURNS JSONB
    LANGUAGE PLPGSQL
    AS $$
      DECLARE
        result JSONB;
      BEGIN
        UPDATE app.plans
          SET name = in_name
            , description = in_description
          WHERE plans.id = in_plan_id
          RETURNING
            json_build_object
              ( 'id'
              , id
              , 'name'
              , name
              , 'description'
              , description
              , 'date'
              , date
              )
          INTO result;

          IF result IS NULL THEN
            RAISE
              'Resource does not exist: %',
              in_plan_id USING ERRCODE = 'no_data_found';
          END IF;

          RETURN result;
      END;
    $$;

  GRANT EXECUTE ON FUNCTION api.edit_plan TO hp_user;

  CREATE OR REPLACE FUNCTION api.delete_plan(in_plan_id UUID)
    RETURNS JSONB
    LANGUAGE PLPGSQL
    AS $$
      DECLARE
        result JSONB;
      BEGIN
        DELETE
          FROM app.plans
          WHERE plans.id = $1
          RETURNING json_build_object
            ( 'id'
            , id
            , 'name'
            , name
            , 'description'
            , description
            , 'date'
            , date
            )
          INTO result;

        IF result IS NULL THEN
          RAISE
            'Resource does not exist and cannot be deleted: %',
            in_plan_id USING ERRCODE = 'no_data_found';
        END IF;

        RETURN result;
      END;
    $$;

  GRANT EXECUTE ON FUNCTION api.delete_plan TO hp_user;

  ------------------------------------------------------------------------------
  -- Event-related functions
  CREATE OR REPLACE FUNCTION api.create_event
    ( in_plan_id UUID
    , in_name TEXT
    , in_start_time TIMESTAMPTZ
    , in_end_time TIMESTAMPTZ
    )
    RETURNS JSONB
    LANGUAGE PLPGSQL
    AS $$
      DECLARE
        same_date BOOLEAN;
        result JSONB;
      BEGIN
        SELECT exists(
          SELECT *
            FROM app.plans
            WHERE plans.id = $1
              AND plans.date = $3 :: DATE
              AND plans.date = $4 :: DATE
        )
          INTO same_date;

        IF NOT same_date THEN
          RAISE 'Resource does not exist: %',
              in_plan_id USING ERRCODE = 'no_data_found';
        END IF;

        INSERT
          INTO app.events (plan_id, name, start_time, end_time, user_id)
          VALUES ($1, $2, $3 :: TIMESTAMP, $4 :: TIMESTAMP, app.current_user_id())
          RETURNING
            json_build_object
              ( 'id'
              , id
              , 'plan_id'
              , plan_id
              , 'start_time'
              , start_time
              , 'end_time'
              , end_time
              , 'user_id'
              , user_id
              , 'name'
              , name
              )
          INTO result;

        RETURN result;
      END;
    $$;

  GRANT EXECUTE ON FUNCTION api.create_event TO hp_user;

  CREATE OR REPLACE FUNCTION api.edit_event
    ( event_id UUID
    , name TEXT
    , start_time TIMESTAMPTZ
    , end_time TIMESTAMPTZ
    )
    RETURNS JSONB
    LANGUAGE PLPGSQL
    AS $$
      DECLARE
        result JSONB;
      BEGIN
        UPDATE app.events
          SET name = $2
            , start_time = $3
            , end_time = $4
          WHERE events.id = $1
          RETURNING
            json_build_object
              ( 'id'
              , events.id
              , 'plan_id'
              , events.plan_id
              , 'start_time'
              , events.start_time
              , 'end_time'
              , events.end_time
              , 'name'
              , events.name
              , 'user_id'
              , user_id
              )
          INTO result;

        IF result IS NULL THEN
          RAISE
            'Resource does not exist: %',
            event_id USING ERRCODE = 'no_data_found';
        END IF;

        RETURN result;
      END;
    $$;

  GRANT EXECUTE ON FUNCTION api.edit_event TO hp_user;

  CREATE OR REPLACE FUNCTION api.delete_event(event_id UUID)
    RETURNS JSONB
    LANGUAGE PLPGSQL
    AS $$
      DECLARE
        result JSONB;
      BEGIN
        DELETE
          FROM app.events
          WHERE events.id = $1
          RETURNING
            json_build_object
              ( 'id'
              , events.id
              , 'plan_id'
              , events.plan_id
              , 'name'
              , events.name
              , 'start_time'
              , events.start_time
              , 'end_time'
              , events.end_time
              )
          INTO result;

        IF result IS NULL THEN
          RAISE
            'Resource does not exist: %',
              event_id USING ERRCODE = 'no_data_found';
        END IF;

        RETURN result;
      END;
    $$;

  GRANT EXECUTE ON FUNCTION api.delete_event TO hp_user;

  ------------------------------------------------------------------------------

  CREATE OR REPLACE FUNCTION api.create_comment(plan_id UUID, content TEXT)
    RETURNS JSONB
    LANGUAGE SQL
    AS $$
      INSERT
        INTO app.comments (plan_id, user_id, content)
        VALUES
          ( create_comment.plan_id
          , app.current_user_id()
          , create_comment.content
          )
        RETURNING
          json_build_object
            ( 'id'
            , id
            , 'content'
            , content
            , 'user_id'
            , user_id
            , 'plan_id'
            , plan_id
            , 'created_at'
            , created_at
            );
    $$;

  GRANT EXECUTE ON FUNCTION api.create_comment TO hp_user;

  CREATE OR REPLACE FUNCTION api.edit_comment(comment_id UUID, content TEXT)
    RETURNS JSONB
    LANGUAGE SQL
    AS $$
      UPDATE app.comments
        SET content = edit_comment.content
        WHERE id = edit_comment.comment_id
        RETURNING
          json_build_object
            ( 'id'
            , id
            , 'content'
            , content
            , 'user_id'
            , user_id
            , 'plan_id'
            , plan_id
            , 'created_at'
            , created_at
            );
    $$;

  GRANT EXECUTE ON FUNCTION api.edit_comment TO hp_user;

  CREATE OR REPLACE FUNCTION api.delete_comment(comment_id UUID)
    RETURNS JSONB
    LANGUAGE SQL
    AS $$
      DELETE
        FROM app.comments
        WHERE id = delete_comment.comment_id
        RETURNING
          json_build_object
            ( 'id'
            , id
            , 'content'
            , content
            , 'user_id'
            , user_id
            , 'plan_id'
            , plan_id
            , 'created_at'
            , created_at
            );
    $$;

  GRANT EXECUTE ON FUNCTION api.delete_comment TO hp_user;

COMMIT;
