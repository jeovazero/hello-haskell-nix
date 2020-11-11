-- Revert hello:0002-add-users from pg

BEGIN;

DROP TABLE hello.users;

DROP EXTENSION "uuid-ossp";

COMMIT;
