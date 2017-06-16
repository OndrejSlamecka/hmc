What is `hmc`
-------------

`hmc` is a
[Music Player Daemon](https://www.musicpd.org/)
client with a simple interface inspired by
[billy](http://www.sheepfriends.com/index-page=billy.html).

Warning: This program is unfinished, work is in progress. See TODO below for a
list of key features missing.

Install
-------

Currently you have to build it yourself with [stack](https://docs.haskellstack.org/en/stable/README/).

    git clone https://github.com/OndrejSlamecka/hmc.git && cd hmc
    stack build
    stack exec hmc

Controls
--------

* Arrows to move in lists, enter to select/play, spacebar to (un)pause, `Tab` to
  play the next song.
* In playlist view: arrows to seek in the song.
* Switch between views: to playlist `F2` or `Esc`, to add `F3`, to open `F4`.
* Exit with `q` or `^d`.
* Update MPD database with `F5`.
* `gg`, `G` to move to the top/bottom of a list.


![Playlist view](screenshot.jpg)


TODO
----

* Search with `/`.
* Playing queue (like billy has).
* Remember directory traversal in browser between `hmc` runs.
* Faster scrolling with shift-arrows, `^f`/`PageDown` and `^b`/`PageUp`.
