# What is this?

This is a web-server for news web-site, written using Haskell and PostgreSQL.
Its basic structure is Haskell application, that gets HTTP requests, makes
some queries to the database and forms a response.

# How it works?

## Main loop

The application has two modes: main mode and migrations mode. The last one is described below
in the "Create database" section.

The main mode is the infinite loop:
1. Get HTTP request
2. Define the action type using the request (routing)
3. Perform the action using the database and get some result (or throw exception)
4. Convert result or exception to the response
5. Respond

The first and the last steps are totally the functions of WARP library.
So, the type of the main function is something like `Request -> IO Response`.
The intermediate steps are discussed further, but somewhat approximately.
Some types may not fully match, for example, but the main semantics is fully explained.

### Define the action type (routing)
All the sources for this subsection can be found in the `Action` directory, and
the main module of it is named `RequestToAction.hs`, which contains the function
`requestToAction` with a type of `Request -> Either ActionError Action`.
The input path is analyzed step by step. If the right path is found, server tries to
convert input parameters list into one of action types, which can be found in `Types` directory
(see, for example, `Types/Category.hs` or `Types/Posts.hs`). After the path is matched, convertion
to action is performed using the Router monad, which type is
```
data RoutingEnv = RoutingEnv {
    _re_admin :: Bool,
    _re_hash :: Query
    } deriving (Show, Generic)
newtype Router a = Router { unR :: ReaterT RoutingEnv (Either ActionError) a }
```
When Router is run, it gets two parameters. `_re_admin` parameter says whether current
action is supposed to be used only for admins. This parameter was introduced, because
when some error occurs (invalid parameter value, for example), the resulting response
depends on whether the user is admin or not.
`_re_hash` is just input parameters string
converted to hashmap for convenience.

If routing returned an error, it is handled inside `handleError` function (module `Execute.hs`).
If user is not admin and action required admin permissions, InvalidEndpoint is always returned.
Otherwise the description of error is sent away.

### Perform the action and obtain the result

After server gets an action value, it executes it using `executeAction` function (module `Execute.hs`).
This function calls other `execute<...>` functions, and all of them form something like a tree.
After we get to one of the bottom leaves, the action is performed.
For some actions the permissions check is firstly done (see `withAuthAdmin`, `withAuthor`, etc. functions, module `Execute/Utils.hs`).

Here is the point where handle pattern is introduced. We need to write code that can be later checked without
external dependencies (like database). So functions that require database operations explicitly are moved to handle,
which is described in the `App/Database.hs` module.
Other functions that contain some logic and use handle functions can be found in `Execute.hs` and `Execute/` directory.
See, for example, `Execute/Draft.hs`.

All the handle functions get the action type value and return either some entities or entities id, if no error occurs.
All the errors are splitted into logic errors and technical errors. If a logical error occurs, it is returned
as `Left err` from handle functions. If technical error occur, it can be handled only in the highest layer, so
it is thrown as an exception. All of the exception are described in the `Exceptions.hs` module, logic errors
are described in the `Types/APIErrors.hs` module.

The "production" implementation of handle function is described in the `App/Database/Postgres.hs` and `IO/Postgres.hs` modules.
All the functions there hardly refer to the SQL and, in particular, PostgreSQL.
For tests some "dummy" implementations are made for tests (see `tests` directory).

`IO/Postgres.hs` contains all the functions that explicitly call the database. For most common actions
(create, read, update, delete of any entity) there are five functions: getThis, getThisPaginated,
createThis, editThis, deleteThis. The details of how this functions work with different entities
are the subject of respective typeclasses, Read, CreateSQL, UpdateSQL, DeleteSQL. They can be found
in the `Database` directory. Each instance defines how to build a query for each entity and some
other auxiliary functions. Note that these modules are stuck with the structure of the real database
and should be rewritten if some other database is used.
Also, for drafts and posts API the HasTags class was used..


### Convert result to the response

See `toResponse` function (module `Lib.hs`) for APIResult to Response convertion
and `mainErrorHandler` (`Exceptions.hs`) for ServerException to Response convertion.

## Logging

Logging is also implemented as a handle, that is passed to all other functions.
Its type is `Priority -> Text -> IO ()`. For initial steps or migrations stderr
logger is used, and for the other part of the application the self-sufficient logger
is used. If any exception is raised inside it, it is handled inside it too.
If it becomes impossible to continue logging to file, the logger starts using
stderr handle.

## Auxiliary modules

* GenericPretty.hs is used for pretty logging;
* RunOptions.hs is used for getting run options from command line arguments;
* Config.hs is used for reading the configuration files;
* Result.hs is used for storing messages for responses;
* Utils.hs includes some useful functions that have no application-logic sense only by themselves.

## Database

The structure of database tables is rather simple. There are some "primary" entities that
exist independently (like tags, users, categories), and other entities refer them (drafts, posts, authors).
The correctness of that database contents is keeped using constraints.

There are only two (in compare to other) difficulties in all this stuff. The first thing is categories,
which are supposed to fetched recursively. So each row of category contains a reference to the parent,
and getting categories uses not very obvious fixed-point semantics, that is often used in Haskell, too.
See migrations/013.sql.

Second thing is tags and posts (drafts) relationship, which can be defined as "many-to-many relationship".
It's optimal to have well normalized database, so for this case besides two tables for entities itself
the third table for post-tag relation is used. The getting operation uses joins between these tables.

# How to get it working?

## Compile

First of all, you need to clone the repository with the server sources and build it using stack.
Usually it is enough to just run `stack build`, all the libraries will be compiled automatically.
Eventually building can crash because of some external libraries that are not installed on your machine.
If you are using Linux, you can just install them yourself. For example, if you are using Ubuntu and
the pq library is missing, just install `libpq-dev` package using `# apt install libpq-dev`. Generally, for most cases if (libname) is
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
`pg_hba.conf`:

`local   databaseName    databaseApp     password`
`local   databaseName    databaseOwner   password`

## Tests

Now we are ready to work.
One may want to make sure that the system is works properly. For this some functionality tests are written
using python3, you can find them inside `curl/final_tests` directory. Of course, you need to have a
python3 interpreter, the path to it is defined on the first line of all test scripts.

There are tests for all entities: authors, categories, comments, drafts, posts, tags, users.
The respective tests scripts has `.py` suffix at the end. The aggregate script is testall.sh.
All tests inspect server response if it is successfull, but do not distinguish one server
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

Configuration file should contain database data and port numero.
The template of configuration file follows:
```
database_name = "newsdb"
app_user = "newsdb_app"
app_user_password = "0000"
admin = "newsdb_owner"
admin_password = "0000"
port = 5555
```

All parameters are required, no optional parameters.

# Command line options

The first command-line option is always path to the configuration file.
The order of subsequent options can be arbitrary.

+ `-m` is used to perform the migrations.
+ `--test-config` is used to test server configuration. Data from config file will be get and then
process will terminate with success exitcode.
+ `-l` is used to define logging settings. Currently logger can only log messages with
given priority or higher. For example, to log Warning, Error and Fatal messages use
`-lWarning`. The default is `-lDebug`, which logs all messages.
+ `--logpath=<path>` is used to define the path to the log file.

