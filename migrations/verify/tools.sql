-- Verify hello:tools on pg

BEGIN;

SELECT has_function_privilege('uuid_generate_v4()', 'execute');
SELECT tool_id, name, description from hello.tools where false;
SELECT name, tool_id from hello.tags where false;

ROLLBACK;
