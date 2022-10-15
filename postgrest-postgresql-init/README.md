# PostgREST-PostgreSQL-init

## Install PostgreSQL

- <https://www.postgresql.org/download/linux/ubuntu/>

## Install PostgREST

- <https://postgrest.org/en/stable/>

```sh
# download from https://github.com/PostgREST/postgrest/releases/latest
tar xJf postgrest-<version>-<platform>.tar.xz
```

or

```sh
git clone https://github.com/PostgREST/postgrest
cd postgrest
stack build
stack install
```

## How to connect PostgreSQL in Ubuntu

```sh
$ sudo apt install postgresql-client-14
$ psql -h localhost -p 5432 -d postgres -U postgres
```


## How to create database for API

```sh
create schema api;
create table api.todos (
  id serial primary key,
  done boolean not null default false,
  task text not null,
  due timestamptz
);

insert into api.todos (task) values
  ('finish tutorial 0'), ('pat self on back');
create role web_anon nologin;

grant usage on schema api to web_anon;
grant select on api.todos to web_anon;
create role authenticator noinherit login password 'mysecretpassword';
grant web_anon to authenticator;
```

## How to run PostgREST

```sh
postgrest tutorial.conf
```

## References

- <https://postgrest.org/en/stable/tutorials/tut0.html>

