-- Verify hello:0002-add-users on pg

BEGIN;

SELECT has_function_privilege('uuid_generate_v4()', 'execute');
SELECT name, user_id, email, password from hello.users where false;

ROLLBACK;
