-- Revert hello:0003-tools-and-tags from pg

BEGIN;

DROP TABLE hello.tags;

DROP TABLE hello.tools;

COMMIT;
