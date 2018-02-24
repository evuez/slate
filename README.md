# slate - a note taking tool.

A simple tool to take notes from your terminal (and sync them between your devices).

Generates markdown [task lists](https://help.github.com/articles/about-task-lists/).

Lists are stored in `~/.config/slate/` and their default name is the name of your current directory. You can use any other name you want using the `--name` option.

## Install

```shell
$ stack install slate
```

## Basic usage

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
  status                   Display the status of a slate.
  sync                     Sync every slate.

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

$ slate status
1 done, 1 todo (2 total).

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

## The `sync` command

You can use `slate sync` to synchronize your slates. There's no default configuration for this command, you'll have to create the file `~/.config/slate/config.toml` and add your sync command, for example:

```toml
sync = "git add . && git commit -m 'Update slates'; git pull --rebase origin master && git push origin master"
```

This would stage & commit every updates in `~/.config/slate/`, update your local copy and push your updates to the `origin` remote.
