-- Deploy holiplan:extensions.sql to pg

BEGIN;

CREATE EXTENSION pgtap;
CREATE EXTENSION btree_gist;
CREATE EXTENSION citext;
CREATE EXTENSION pgcrypto;

COMMIT;
