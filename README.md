# filmsearch
Search films in TV-channels

- write some IMDB-IDs to /tmp/imdb-ids
- call imdb2fs.lisp
- add the resulting filmsearch-entries in /tmp/fs-entries to your personal
  list in /home/user/fs-entries
- write your configuration to /home/user/.filmsearch

cronjob (or at every start of vdr):
"filmsearch.lisp /home/user/.filmsearch"

ToDo:
- filmsearch.lisp (check-title): optimize (pre-compiled regular expressions)
- imdb2fs.lisp: add support for TV Series
