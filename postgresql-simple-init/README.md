# postgresql-simple-init

## Prerequisite

```sql
CREATE TABLE test (
       id int
       , name varchar
);

DROP TABLE test;
```

### In Ubuntu

```sh
sudo apt install -y libpq-dev
```

### In Manjaro

```sh
sudo pacman -S postgresql-libs
```

### Add a package dependency

```yaml
dependencies:
- postgresql-simple
```

## References

- <https://tuttlem.github.io/2020/10/30/postgresql-data-access-with-haskell.html>
