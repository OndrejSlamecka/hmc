What is `hmc`
-------------

`hmc` is a
[Music Player Daemon](https://www.musicpd.org/)
client with a simple interface inspired by
[billy](http://www.sheepfriends.com/index-page=billy.html).

Warning: This program is in an early stage of development.

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
* Pause and exit with `q` or `^d`, exit without pausing with `^q` (Ctrl-q).
* Update MPD database with `F5`.
* `gg`, `G` to move to the top/bottom of a list, `^f`/`PageDown` and `^b`/`PageUp` to scroll by pages, shift-arrows to scroll by 5 items.
* Search with `/`, leave by `Esc`. In browser the search shows items of
  which the searched string is a part of, also right arrow (still) enters the
  selected directory.


![Playlist view](screenshot.jpg)


TODO
----

* Playing queue (like billy has).
