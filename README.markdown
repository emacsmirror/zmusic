# zmusic

Write music inside Emacs! This currently works on Linux only, as it requires [aplay](https://linux.die.net/man/1/aplay).

## Starting

Begin with `M-x zmusic`. This shows a series of lines. Each line of text represents a musical beat. The first beat should be highlighted in blue. Each dash in that line represents a potential note in that beat.

## Navigation

Move around with `p`/`n`/`b`/`f`.

## Music creation

Toggle the note at point with `space`.

Kill the current beat with `k`, and yank it with `y`.

## Playing music

Play music, or pause the music, with `P`.

Set the next note to the one at point with `s`.

If the music is paused, replay the currently-highlighted beat with `r`.

Count the highlighted beat, playing it and steping forwards, with `c`.
