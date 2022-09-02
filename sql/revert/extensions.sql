-- Revert holiplan:extensions.sql from pg

BEGIN;

DROP EXTENSION pgtap;
DROP EXTENSION btree_gist;
DROP EXTENSION citext;
DROP EXTENSION pgcrypto;

COMMIT;
