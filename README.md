# emir
dsl for package building/installing things. made it to learn nim because i like nim now its so peak ngl.

## installing
requirements: nim and nimble.
just run `sudo apt install nim` (or your equivalent distros package manager).

`nim c -d:release emir.nim` builds emir
(if any missing files show up just install the dependencies with nimble)

## syntax
- variables - global variables for reusing across stages: eg `VERSION = "1.0.0"`
- stages - basically like a make target but not the same (sorry im rushing this readme lmao)
```
stage install
  +file "test"
  >> "created test file"
```
heres an example stage

## features
- exec: runs shell commands. (no sanitisizing and its through your system shell ALWAYS dry run first)
- fetch: native downloading. `fetch "url" -> "destination"`
- filesystem control: use `+dir`, `-dir`, `+file`, and `-file` for adding and removing dirs and files.
- logging: use `>>` to add ur own logs.
- smart caching: use the `hash` keyword in your stage declaration. if the target file hasnt changed, emir skips the stage

## why i made it
to learn nim i love nim its so good no cap on god bro (holy the boys refrence if yk yk) umm idk bro for fun
## notices
- it is ai assisted i would say 70% my code 30% ai (for helping with some boilerplate) i still dont know how to use macros but i will learn
- the ai is local so like i guess im not killing the polar bears guys
- by installing packages your giving the author full abritary code execution please PLEASE dry run ATLEAST to check if its not gonna destroy everything xd
- its made for fun you can use it if you want but there are most likely better alternatives its not made to replace make etc
- have fun using it ig lol
