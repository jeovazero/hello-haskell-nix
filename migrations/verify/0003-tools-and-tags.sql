-- Verify hello:0003-tools-and-tags on pg

BEGIN;

SELECT tool_id, name, user_id description from hello.tools where false;
SELECT name, tool_id from hello.tags where false;

ROLLBACK;
