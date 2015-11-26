# filmsearch
Search EPG for films or other programs.

Example usage:

- write some IMDB-IDs to /tmp/imdb-ids
- call imdb2fs.lisp
- add the resulting filmsearch-entries in /tmp/fs-entries to your personal
  list in /home/user/fs-entries
- write your configuration to /home/user/.filmsearch

cronjob (or at every start of vdr): "filmsearch.lisp /home/user/.filmsearch"

For example, I have these lines in my run-vdr-script, just before calling VDR:

    pidof sbcl >/dev/null || nice sudo -upeter ~peter/filmsearch.lisp \
          ~peter/fs-config &>>/dev/shm/fs.log &

ToDo:

- imdb2fs.lisp: add support for TV Series
