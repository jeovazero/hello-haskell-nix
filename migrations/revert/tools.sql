-- Revert hello:tools from pg

BEGIN;

DROP TABLE hello.tags;

DROP TABLE hello.tools;

DROP EXTENSION "uuid-ossp";

COMMIT;
