BEGIN;
  SELECT * FROM plan(16);

  ------------------------------------------------------------------------------
  -- Only `hp_user` should be able to execute the aforementioned functions:

  SELECT function_privs_are(
    'api',
    'create_plan',
    ARRAY['text', 'text', 'date', 'text', 'text'],
    'hp_user',
    ARRAY['EXECUTE'],
    'hp_user should be able to execute create_plan/5'
  );

  SELECT function_privs_are(
    'api',
    'get_plan_details',
    ARRAY['uuid'],
    'hp_user',
    ARRAY['EXECUTE'],
    'hp_user should be able to execute get_plan_details/1'
  );

  SELECT function_privs_are(
    'api',
    'list_plans',
    '{}',
    'hp_user',
    ARRAY['EXECUTE'],
    'hp_user should be able to execute list_plans/0'
  );

  SELECT function_privs_are(
    'api',
    'edit_plan',
    ARRAY['uuid', 'text', 'text'],
    'hp_user',
    ARRAY['EXECUTE'],
    'hp_user should be able to execute edit_plan/3'
  );

  SELECT function_privs_are(
    'api',
    'delete_plan',
    ARRAY['uuid'],
    'hp_user',
    ARRAY['EXECUTE'],
    'hp_user should be able to execute delete_plan/1'
  );

  SELECT function_privs_are(
    'api',
    'create_event',
    ARRAY['uuid', 'text', 'timestamp without time zone', 'timestamp without time zone'],
    'hp_user',
    ARRAY['EXECUTE'],
    'hp_user should be able to execute create_event/4'
  );

  SELECT function_privs_are(
    'api',
    'edit_event',
    ARRAY['uuid', 'text', 'timestamp without time zone', 'timestamp without time zone'],
    'hp_user',
    ARRAY['EXECUTE'],
    'hp_user should be able to execute edit_event/4'
  );

  SELECT function_privs_are(
    'api',
    'delete_event',
    ARRAY['uuid'],
    'hp_user',
    ARRAY['EXECUTE'],
    'hp_user should be able to execute delete_event/1'
  );

  ------------------------------------------------------------------------------
  -- `hp_anon` cannot execute any of the aforemention functions.

  SELECT function_privs_are(
    'api',
    'create_plan',
    ARRAY['text', 'text', 'date', 'text', 'text'],
    'hp_anon',
    '{}',
    'hp_anon should not be able to execute create_plan/5'
  );

  SELECT function_privs_are(
    'api',
    'get_plan_details',
    ARRAY['uuid'],
    'hp_anon',
    '{}',
    'hp_anon should not be able to execute get_plan_details/1'
  );

  SELECT function_privs_are(
    'api',
    'list_plans',
    '{}',
    'hp_anon',
    '{}',
    'hp_anon should not be able to execute list_plans/0'
  );

  SELECT function_privs_are(
    'api',
    'edit_plan',
    ARRAY['uuid', 'text', 'text'],
    'hp_anon',
    '{}',
    'hp_anon should not be able to execute edit_plan/3'
  );

  SELECT function_privs_are(
    'api',
    'delete_plan',
    ARRAY['uuid'],
    'hp_anon',
    '{}',
    'hp_anon should not be able to execute delete_plan/1'
  );

  SELECT function_privs_are(
    'api',
    'create_event',
    ARRAY['uuid', 'text', 'timestamp without time zone', 'timestamp without time zone'],
    'hp_anon',
    '{}',
    'hp_anon should not be able to execute create_event/4'
  );

  SELECT function_privs_are(
    'api',
    'edit_event',
    ARRAY['uuid', 'text', 'timestamp without time zone', 'timestamp without time zone'],
    'hp_anon',
    '{}',
    'hp_anon should not be able to execute edit_event/4'
  );

  SELECT function_privs_are(
    'api',
    'delete_event',
    ARRAY['uuid'],
    'hp_anon',
    '{}',
    'hp_anon should not be able to execute delete_event/1'
  );

  SELECT * FROM finish();

ROLLBACK;
