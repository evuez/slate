# slate - a note taking tool.

A simple tool to take notes from your terminal.

Generates markdown [task lists](https://help.github.com/articles/about-task-lists/).
Lists are stored in `~/.config/slate/`.

## Install

```shell
$ stack install
```

## Usage

<pre>
$ slate --help

slate - a note taking tool.

Usage: slate COMMAND
  Slate

Available options:
  -h,--help                Show this help text

Available commands:
  add                      Add a note.
  check                    Check a note.
  uncheck                  Uncheck a note.
  remove                   Remove a note.
  display                  Display a slate.
  rename                   Rename a slate.
  wipe                     Wipe a slate.

$ slate add "My first note."
$ slate add "New note!"
$ slate display
00 - My first note.
01 - New note!

$ slate check 0
$ slate display
<s>00 - My first note.</s>
01 - New note!

$ slate display --only=unchecked
01 - New note!

$ slate add "Fake note"
$ slate display
<s>00 - My first note.</s>
01 - New note!
02 - Fake note

$ slate remove 2
$ slate display
<s>00 - My first note.</s>
01 - New note!

$ slate wipe --only=unchecked
$ slate display
<s>00 - My first note.</s>

$ slate uncheck 0
$ slate display
00 - My first note.
</pre>
