---
title: "Notes (HPFP 31/31): Final Project"
---

# 31 Final Project

## 31.3 Exploring finger

On nixos, install

```
$ nix-env -iA nixos.netbsd.finger
```

To run the debug program as root:

```
$ stack exec sudo debug
```

makes more senes than what's in the book.


## 31.5 Chapter Exercises

### 1

A useful command to see the table:

```
sqlite> .dump users
PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
CREATE TABLE users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT, homeDirectory TEXT,
   realName TEXT, phone TEXT);
INSERT INTO users VALUES(1,'callen','/bin/zsh','/home/callen','Chris Allen','555-123-4567');
INSERT INTO users VALUES(2,'jcb','/bin/zsh','/home/jcb','John Burnham','123-456-789');

```

To insert a new row:

```
sqlite> insert into users values(2,'jcb','/bin/zsh','/home/jcb','John Burnham','123-456-789');
sqlite> .dump users
PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
CREATE TABLE users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT, homeDirectory TEXT,
   realName TEXT, phone TEXT);
INSERT INTO users VALUES(1,'callen','/bin/zsh','/home/callen','Chris Allen','555-123-4567');
INSERT INTO users VALUES(2,'jcb','/bin/zsh','/home/jcb','John Burnham','123-456-789');
```

To modify an existing user:

```
sqlite> update users set username = 'john' where id = 2;
sqlite> .dump users
PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
CREATE TABLE users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT, homeDirectory TEXT,
   realName TEXT, phone TEXT);
INSERT INTO users VALUES(1,'callen','/bin/zsh','/home/callen','Chris Allen','555-123-4567');
INSERT INTO users VALUES(2,'john','/bin/zsh','/home/jcb','John Burnham','123-456-789');
```

## 2

see `src/UserAdd.hs`


