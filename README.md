# luafmt
A code formatter for Lua 5.1, written in Lua 5.1.

Usage:

To print the formatted version to standard out:

    lua luafmt.lua <lua file> [max column hint=80]
    
To update the Lua file in place:
    
    lua luafmt.lua --f <lua file> [max column hint=80]

`luafmt.lua` in this repository is formatted using itself.

* No multiple blank lines in a row
* No spaces on blank lines
* Spaces around operators and assignment
* Space around string and table calls: `print {}`, `print ""`
* Space after commas: `return {1, 2}`
* Indentation using tabs for both scope and overflowed lines
* Blank lines above comments
* No blanks at the beginning or ends of blocks

A bash script like the following can be used to reformat all `.lua` files in a directory:

     for f in *.lua ; do lua luafmt.lua --f "$f" ; done

Scripts which are not syntactically correct may not be formattable. The script
will output an error and not modify the file.

# Testing

Run `lua test.lua` from the project directory to run tests.

Right now, there are only a couple of tests.
