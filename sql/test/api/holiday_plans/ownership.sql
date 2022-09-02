BEGIN;
  SELECT * FROM plan(8);

  ------------------------------------------------------------------------------
  -- `hp_api` should own all objects under the API schema.
  SELECT function_owner_is(
    'api',
    'create_plan',
    ARRAY['text', 'text', 'date', 'text', 'text'],
    'hp_api',
    'hp_api owns the function create_plan/5'
  );

  SELECT function_owner_is(
    'api',
    'get_plan_details',
    ARRAY['uuid'],
    'hp_api',
    'hp_api owns the function get_plan_details/1'
  );

  SELECT function_owner_is(
    'api',
    'list_plans',
    '{}',
    'hp_api',
    'hp_api owns the function list_plans/0'
  );

  SELECT function_owner_is(
    'api',
    'edit_plan',
    ARRAY['uuid', 'text', 'text'],
    'hp_api',
    'hp_api owns the function edit_plan/3'
  );

  SELECT function_owner_is(
    'api',
    'delete_plan',
    ARRAY['uuid'],
    'hp_api',
    'hp_api owns the function delete_plan/1'
  );

  SELECT function_owner_is(
    'api',
    'create_event',
    ARRAY['uuid', 'text', 'timestamp without time zone', 'timestamp without time zone'],
    'hp_api',
    'hp_api owns the function create_event/4'
  );

  SELECT function_owner_is(
    'api',
    'edit_event',
    ARRAY['uuid', 'text', 'timestamp without time zone', 'timestamp without time zone'],
    'hp_api',
    'hp_api owns the function edit_event/4'
  );

  SELECT function_owner_is(
    'api',
    'delete_event',
    ARRAY['uuid'],
    'hp_api',
    'hp_api owns the function delete_event/4'
  );

  SELECT * FROM finish();
ROLLBACK;
