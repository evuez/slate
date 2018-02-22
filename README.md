# slate - a note taking tool.

A simple tool to take notes from your terminal.

Generates markdown [task lists](https://help.github.com/articles/about-task-lists/).

Lists are stored in `~/.config/slate/` and their default name is the name of your current directory. You can use any other name you want using the `--name` option.

## Install

```shell
$ stack install slate
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
  done                     Mark a note as done when given a note ID, display
                           done notes otherwise.
  todo                     Mark a note as todo when given a note ID, display
                           todo notes otherwise.
  remove                   Remove a note.
  display                  Display a slate.
  rename                   Rename a slate.
  wipe                     Wipe a slate.

$ slate add "My *first* note."
$ slate add "New note!"
$ slate display
00 - My <b>first</b> note.
01 - New note!

$ slate done 0
$ slate display
<s>00 - My <b>first</b> note.</s>
01 - New note!

$ slate display --only=todo # or just slate todo
01 - New note!

$ slate add "Fake note"
$ slate display
<s>00 - My <b>first</b> note.</s>
01 - New note!
02 - Fake note

$ slate remove 2
$ slate display
<s>00 - My <b>first</b> note.</s>
01 - New note!

$ slate wipe --only=todo
$ slate display
<s>00 - My <b>first</b> note.</s>

$ slate todo 0
$ slate display
00 - My <b>first</b> note.
</pre>
