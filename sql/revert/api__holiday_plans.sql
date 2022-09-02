-- Revert holiplan:api__holiday_plans from pg

BEGIN;

  REVOKE EXECUTE ON FUNCTION api.create_plan FROM hp_user;
  REVOKE EXECUTE ON FUNCTION api.get_plan_details FROM hp_user;
  REVOKE EXECUTE ON FUNCTION api.list_plans FROM hp_user;
  REVOKE EXECUTE ON FUNCTION api.edit_plan FROM hp_user;
  REVOKE EXECUTE ON FUNCTION api.delete_plan FROM hp_user;
  REVOKE EXECUTE ON FUNCTION api.create_event FROM hp_user;
  REVOKE EXECUTE ON FUNCTION api.edit_event FROM hp_user;
  REVOKE EXECUTE ON FUNCTION api.delete_event FROM hp_user;
  REVOKE EXECUTE ON FUNCTION api.create_comment FROM hp_user;
  REVOKE EXECUTE ON FUNCTION api.edit_comment FROM hp_user;
  REVOKE EXECUTE ON FUNCTION api.delete_comment FROM hp_user;

  DROP FUNCTION api.delete_comment;
  DROP FUNCTION api.edit_comment;
  DROP FUNCTION api.create_comment;
  DROP FUNCTION api.delete_event;
  DROP FUNCTION api.edit_event;
  DROP FUNCTION api.create_event;
  DROP FUNCTION api.delete_plan;
  DROP FUNCTION api.edit_plan;
  DROP FUNCTION api.list_plans;
  DROP FUNCTION api.get_plan_details;
  DROP FUNCTION api.create_plan;

COMMIT;
