# filmsearch
Search films in TV-channels

done-marker: ==============================================================

- manually:      select imdb-id
==============================================================
- automatically: fill structure with data of imdb-id and level-change-date
- manually:      adjust the content of the structure and add it to db

cronjob (or at every start of vdr):
- if epg-data has changed, update epg-structure (only channels in channel-list)
  - new entries are added with "already-searched = 0"
  - if old entries have changed, they get also "already-searched = 0"
- do the search
  - for each epg-entry "already-searched = 0", check against each db-entry:
    - level = 1: db-title must be in epg-title (orig or same lang)
    - level = 2: db-title must match epg-title exactly (orig or same lang)
    - level = 3: level 2 + epg-duration > 80% * db-duration
    - level = 4: level 3 + db-year - 1 <= epg-year <= db-year + 1
    - level = 5: level 4 + epg-description contains at least one
                 actor/writer/director
    - if match is found, send email:
      - all epg-data
      - all db-data
      - link to vdr-admin for creating timer
      - link to imdb
      - line in db (for telling emacs to jump to db-file at this line)
    - increment match-counter of db-entry
    - set "already-searched = 1"
- autoadapt search levels:
  - for each db-entry
    - if "autoadapt = 1" and last level-change is more than 5 weeks ago and
      matches per week > 2:
      increment level, update level-change-date and reset match-counter
    - if last level-change is more than 150 weeks ago and matches per week = 0:
      send email about missing matches
