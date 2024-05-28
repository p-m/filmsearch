# FilmSearch
Search the EPG of VDR for films or other programs.

When could this be useful for you?
- You are a VDR user.
- You want to get notifications, when there are specific TV programs.

## Usage
You need a database with your search entries, for example `~/fs/entries`. The
default configuration file is `~/.config/filmsearch.conf`. If you use another
location, you have to run `filmsearch` with the name of that location as first
argument. It’s useful to run the program from a cronjob: It runs
automatically and crond sends you the output by email.
Examples for the configuration file, the database and a crontab file are in
the `examples` subdirectory. Useful patches are in the `patches` subdirectory.

`imdb2fs` is a helper program for creating database entries from IMDB
identifiers. Its default configuration file is `~/.config/imdb2fs.conf`, but
as with `filmsearch` you can specify another location as first argument.

`make-mkv` converts the edited recordings to smaller MKV files, that can be
distributed (e.g. using MiniDLNA) to Smart-TVs. Its default configuration file
is `~/.config/make-mkv.conf`, but as with `filmsearch` you can specify
another location as first argument.

## Installation
Run `make` to create the executables. If something is missing, you should get
useful error messages.

## Example workflow
- A friend recommends a film.
- Check it on IMDB.
- Add the ID (e.g. tt12345) to your list (e.g. `~/fs/imdb-ids`).
- Call imdb2fs after a while.
- Get email from the FilmSearch cronjob.
- Add the timers (see also the patch for VDRAdmin).
- Adjust the search entries (e.g. `~/fs/entries`), especially when there are too
  many wrong matches.
- Check and edit the new recordings,  (useful plug-in: MarkAd).
- Cut.
- Wait for MKV files becoming available.

LocalWords:  FilmSearch EPG VDR filmsearch cronjob crond crontab imdb2fs
LocalWords:  MiniDLNA tt12345 VDRAdmin MarkAd
