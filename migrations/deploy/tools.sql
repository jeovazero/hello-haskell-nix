-- Deploy hello:tools to pg
-- requires: init-schema

BEGIN;

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE hello.tools (
  tool_id       uuid DEFAULT uuid_generate_v4 (),
  name          VARCHAR(128) NOT NULL UNIQUE,
  description   VARCHAR(512),
  PRIMARY KEY   (tool_id)
);

CREATE TABLE hello.tags (
    name        VARCHAR(128) NOT NULL,
    tool_id     uuid,
    PRIMARY KEY (name, tool_id),
    FOREIGN KEY (tool_id) REFERENCES hello.tools(tool_id) ON DELETE CASCADE
);

COMMIT;
