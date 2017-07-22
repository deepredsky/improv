## Introduction

The given set of inputfiles contains lines in the format [id]:[content]. The 'id' is an int64 and
the 'content' is a string in UTF-8 format with embedded newlines escaped as '\n'. Eg.

```
123:really long line 1
456:really long line 2
```

The lenght of the id varies but the id and content is always separated by a colon ':'. The id
is always the first item on the lines in the files and is always a number. The text content
runs until next linebreak.


## Example

```
$ improv "line \d+" file1 file2

> 123
> 456

```
