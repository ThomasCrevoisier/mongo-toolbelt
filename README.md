# mongo-toolbelt

## Usage

```
> mongo-toolbelt --help

Usage: mongo-toolbelt COMMAND
  Set of helpers to keep your sanity when using MongoDB

Available options:
  -h,--help                Show this help text

Available commands:
  search-id                Search usage of an ID across the database
```

### Searching usage of an ID across the database

```
MONGO_HOST=localhost MONGO_DATABASE=my_database \
	mongo-toolbelt search-id <SOME_OBJECT_ID>
```
