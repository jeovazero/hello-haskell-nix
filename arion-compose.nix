{ pkgs, ... }:
{
  config.services = {
    postgres = {
      service.image = "postgres:12-alpine";
      service.environment.POSTGRES_USER = "dev";
      service.environment.POSTGRES_PASSWORD = "pass";
      service.environment.POSTGRES_DB = "dev";
      service.ports = [ "5444:5432" ];
    };
  };
}

