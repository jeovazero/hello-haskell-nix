-- Deploy hello:0002-add-users to pg
-- requires: 0001-init-schema

BEGIN;

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE hello.users (
  user_id       uuid DEFAULT uuid_generate_v4 (),
	name					VARCHAR(1024) NOT NULL,
  email         VARCHAR(1024) NOT NULL UNIQUE,
	password			VARCHAR(1024) NOT NULL,
  PRIMARY KEY   (user_id)
);

COMMIT;
