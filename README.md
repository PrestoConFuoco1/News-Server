# What is this?

This is a web-server for news web-site, written using Haskell and PostgreSQL.
Its basic structure is Haskell application, that gets HTTP requests, makes
some queries to the database and forms a response.

# How it works?






# How to get it working?

## Compile

First of all, you need to clone the repository with the server sources and build it using stack.
Usually it is enough to just run `stack build`, all the libraries will be compiled automatically.
Eventually building can crash because of some external libraries that are not installed on your machine.
If you are using Linux, you can just install them yourself. For example, if you are using Ubuntu and
the pq library is missing, just install `libpq-dev` package. Generally, for most cases if (libname) is
missing, the package lib(libname)-dev is what you need.

## Create database

After the application is compiled, it still can't work as it needs also a database. Here PostgreSQL
database is used, so you need to install it on you machine.
First of all, you need to create database.
The instruction here supposes that database works on the same machine with web-server;
if it is not true, a little changes should be made: see postgreSQL documentaion.
We will refer to the database name as databaseName.
Next, you will need two users to be created: the owner of database and the user for application.
We will refer to them as to databaseOwner, databaseApp.
So, next commands are to be run using postgreSQL superuser:

```sql
    CREATE DATABASE databaseName;
    CREATE USER databaseOwner PASSWORD <ownerPassword>;
    CREATE USER databaseApp   PASSWORD <appPassword>;
    ALTER DATABASE databaseName OWNER TO databaseOwner;
```
The application gets all the data from the configuration file, so you need to put it there.
See configuration file format below. In this section we will use file config.conf.

After users are created, you need to create the entire database. It is rather easy since
the web-server supports migrations and has a separate command-line argument to apply all the
migrations to the given database. To run migrations, use `stack exec warpt3 -- config.conf -m`.

On this step we are almost ready to work, but there are some details. Firstly, by default, application user
has not enough permissions to use database as it wants. So, we need to run some extra commands
(using superuser or databaseOwner, it doesn't matter):

```sql
    GRANT USAGE ON SCHEMA news TO databaseApp;
    GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA news TO databaseApp;
    GRANT USAGE ON ALL SEQUENCES IN SCHEMA news TO databaseApp;
```

Secondly, you should let PostgreSQL know, who can connect to the database and what authentication method
should be used. If the database is on the same machine with the server, the two lines are to be added to
pg\_hba.conf:

local   databaseName    databaseApp     password
local   databaseName    databaseOwner   password

## Tests

Now we are ready to work.
One may want to make sure that the system is works properly. For this some functionality tests are written
using python3, you can find them inside `curl/final\_tests` directory. Of course, you need to have a
python3 interpreter, the path to it is defined on the first line of all test scripts.

There are tests for all entities: authors, categories, comments, drafts, posts, tags, users.
The respective tests scripts has `.py` suffix at the end. The aggregate script is testall.sh.
All tests inspect server response if it is successfull, but do not distinct distinguish one server
errors from another. If the result is success and should be error, or vice versa, test script prints "False".
The same for situation when the result should be success and is success, but containing wrong data
(hence we can say that application logic is broken). Otherwise, "True" is print.
So, to ensure that logic is OK we can use `./testall.sh | grep` False. This normally should produce no input,
except one line with fragment "admin: False". To ignore it too, use `./testall.sh | grep False | grep -v admin`.

Next, the server should normally return any internal errors (500). To ensure that no such errors is returned,
run `./testall.sh | grep Internal`. Normally it should produce no input.

One more detail. For tests of posts API, you may want to have a number of posts. So, before running all tests,
it is recommended to create some ones; it's possible to do that using `./draft.py` script, which works n times
if called with argument n: `./draft.py n`, and creates n posts with random contents. For example, to get
50 posts run `./draft.py 50`.
For tests, you may also want posts to have different dates of creation. You can do that directly in the database,
for example using command:

```sql
    UPDATE post SET creation_date = '2021-09-01' WHERE post_id <= 25;
```

In the end, if you are interested in what the tests really do, you are welcome to see its sources
and its output (without using grep).

# Configuration file


