# slate - a note taking tool.
[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Fevuez%2Fslate.svg?type=shield)](https://app.fossa.io/projects/git%2Bgithub.com%2Fevuez%2Fslate?ref=badge_shield)


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
  done                     Mark a note as done.
  todo                     Mark a note as to-do.
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
<s>00 - My first note.</s>
01 - New note!

$ slate display --only=todo
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

$ slate wipe --only=todo
$ slate display
<s>00 - My first note.</s>

$ slate todo 0
$ slate display
00 - My first note.
</pre>


## License
[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Fevuez%2Fslate.svg?type=large)](https://app.fossa.io/projects/git%2Bgithub.com%2Fevuez%2Fslate?ref=badge_large)