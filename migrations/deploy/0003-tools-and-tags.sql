-- Deploy hello:0003-tools-and-tags to pg
-- requires: 0002-add-users

BEGIN;

CREATE TABLE hello.tools (
  tool_id       uuid DEFAULT uuid_generate_v4 (),
  name          VARCHAR(128) NOT NULL UNIQUE,
  description   VARCHAR(512),
  user_id       uuid NOT NULL,
  PRIMARY KEY   (tool_id),
  FOREIGN KEY   (user_id) REFERENCES hello.users(user_id) ON DELETE CASCADE
);

CREATE TABLE hello.tags (
    name        VARCHAR(128) NOT NULL,
    tool_id     uuid NOT NULL,
    PRIMARY KEY (name, tool_id),
    FOREIGN KEY (tool_id) REFERENCES hello.tools(tool_id) ON DELETE CASCADE
);

COMMIT;
