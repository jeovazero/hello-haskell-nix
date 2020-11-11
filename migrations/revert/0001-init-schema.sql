-- Revert hello:0001-init-schema from pg

BEGIN;

DROP SCHEMA hello;

COMMIT;
