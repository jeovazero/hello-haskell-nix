-- Verify hello:init-schema on pg

BEGIN;

SELECT pg_catalog.has_schema_privilege('hello', 'usage');

ROLLBACK;
