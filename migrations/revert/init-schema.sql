-- Revert hello:init-schema from pg

BEGIN;

DROP SCHEMA hello;

COMMIT;
