# luafmt
A code formatter for Lua

Usage:

    lua luafmt.lua <lua file> [max column hint]

The script outputs the formatted version to standard output.

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
     for f in *.lua ; do lua luafmt.lua "$f" > tmp && cat tmp > "$f" ; done
