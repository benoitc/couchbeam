couchbeam NEWS
--------------

version 1.4.1 / 2016-09-26
--------------------------

- maintainance update

version 1.4.0 / 2016-09-22
--------------------------

- maintainance update.

version 1.3.1 / 2016-07-01
--------------------------

- fix: accept 202 status in `couchbeam:save_doc/4` function (#144)
- fix: spec syntax to build with Erlang 19 (#145)

version 1.3.0 / 2016-03-22
--------------------------

- add `couchbeam:all_dbs/2`
- add `couchbeam:view_cleanup/1`
- add `couchbeam:design_info/2`
- add `post_decode` function to view stream
- add Elixir mix support
- fix: handle http errors in view stream (#140)
- fix: build with latest rebar3

version 1.2.1 / 2015/11/04
--------------------------

- also support hackney 1.4.4 for rebar2.
- fix hex.pm release to really use 1.4.4

version 1.2.0 / 2015/11/04
--------------------------

- move to eunit for tests.
- hex.pm support
- mix & rebar3 build tools support
- bump hackney to 1.4.4
- bump jsx to 2.2.8

### Breaking change

erlang-oauth is now optionnal and won't be installed by default.

version 1.1.8 / 2015-08-27
--------------------------

- use latest stable branch of [hackney](https://github.com/benoitc/hackney)

version 1.1.7 / 2015-03-11
--------------------------

- bump [hackney](https://github.com/benoitc/hackney) to 1.1.0
- fix `Conten-Type` header ehen posting doc IDS in changes #126
- fix documentation

version 1.1.6 / 2015-01-02
--------------------------

- fix `included_applications` (#122)

version 1.1.5 / 2014-12-09
--------------------------

- improvement: do not force connections options to nodelay
- update to [Hackney](https://github.com/benoitc/hackney) 1.0.4 fix #120
- fix: retry fecthing UUIDS on error (#121)

version 1.1.4 / 2014-12-01
--------------------------

- update to [Hackney](https://github.com/benoitc/hackney) 1.0.1: more SSL
  certificate authority handling.
- fix: changes stream

version 1.1.3 / 2014-11-30
--------------------------

- update to [Hackney](https://github.com/benoitc/hackney) 1.0.0

version 1.1.2 / 2014-11-15
--------------------------

- remove spurious prints

version 1.1.1 / 2014-11-11
--------------------------

- update to [hackney 0.15.0](https://github.com/benoitc/hackney/releases ),
  improving performances and concurrency
- fix `couchbeam:doc_exists/2`(#116)
- fix `couchbeam:reply_att/1 (#114)


version 1.1.0 / 2014-10-28
--------------------------

- update to [hackney 0.14.3](https://github.com/benoitc/hackney/releases)
- fix memory leaks
- correctly close sockets
- fix streaming issue: don't wait the stream timeout to report the initial
  error.
- update JSX dependency to version 2.1.1

version 1.0.7 / 2014-07-08
--------------------------

- bump to [hackney 0.13.0](https://github.com/benoitc/hackney/releases/tag/0.13.0)

version 1.0.6 / 2014-04-18
--------------------------

- bump to [hackney 0.12.1](https://github.com/benoitc/hackney/releases/tag/0.12.2)


version 1.0.5 / 2014-04-18
--------------------------

- improve connections with HTTP proxies
- improve content-types detection of attachments
- improve URL encoding normalzation, useful when connecting to an
  international domain/URI
- URL resolving is faster
- bump to [hackney 0.12.0](https://github.com/benoitc/hackney/releases/tag/0.12.0)

version 1.0.4 / 2014-04-15
--------------------------

- remove spurious print

version 1.0.3 / 2014-04-15
--------------------------

- add support for the `new_edits` option in bulk doc API.
- improvement: send a doc as multipart that already contains attachments
- bump [hackney](http://github.com/benoitc/hackney) to 0.11.2
- fix path encoding

version 1.0.2 / 2014-01-03
--------------------------

- fix: send a doc as multipart that already contains attachments


version 1.0.1 / 2013-12-30
--------------------------

- fix connection reusing in changes and view streams
- bump hackney version to 0.10.1


version 1.0.0 / 2013-12-21
--------------------------

**First stable release**. This is a supported release.

- send a doc and its attachments efficiently using the [multipart
  API](http://docs.couchdb.org/en/latest/api/document/common.html#creating-multiple-attachments).
- add `couchbeam:get_config/{1,2,3}`, `couchbeam:set_config/{4,5}` and
  `couchbeam:delete_config/{3,4}` to use the [config
API](http://docs.couchdb.org/en/latest/api/server/configuration.html).
- add `couchbeam_uuids:random/0` and `couchbeam_uuids:utc_random/0` to
  generate UUIDS in your app instead of reusing the UUID generated on
the node. By default couchbeam is fetching from the node, which is
- add `{error, forbidden}` and `{error, unauthenticated}` as possible
  results of a reply.
better if you want to use UUID based on the node time.
- fix accept header handling


version 0.10.0 / 2013-12-21
---------------------------

- add `couchbeam:copy_doc/{2,3}` to support the COPY API
- add `couchbeam:get_missing_revs/2` to get the list of missing
  revisions
- add support of the [multipart
  API](http://docs.couchdb.org/en/latest/api/document/common.html#efficient-multiple-attachments-retrieving) when fetching a doc: This change make
  `couchbeam:open_doc/3` return a multipart response `{ok, {multipart,
Stream}}` when using the setting `attachments=true` option. A new option
{`accept. <<"multipart/mixed">>}" can also be used with the options
`open_revs` or `revs` to fetch the response as a multipart.
- bump the [hackney](http://github.com/benoitc/hackney) version to
  **0.9.1** .


With this change you can now efficiently retrieve a doc with all of its
attachments or a doc wit all its revisions.
 master

version 0.9.3 / 2013-12-07
--------------------------

- fix: `couchbeam:open_or_create_db/2'

version 0.9.2 / 2013-12-07
--------------------------

- bump hackney version to 0.8.3

version 0.9.1 / 2013-12-05
--------------------------

- fix design docid encoding

version 0.9.0 / 2013-12-05
--------------------------

This is a major release pre-1.0. API is now frozen and won't change much
until the version 1.0.

- replaced the use of `ibrowse` by `hackney` to handle HTTP connections
- new [streaming
  API](https://github.com/benoitc/couchbeam#stream-view-results) in view
- breaking change: remobe
- breaking change: remove deprecated view API. Everything is now managed in the
  [couch_view](https://github.com/benoitc/couchbeam/blob/master/doc/couchbeam_view.md) module.
- replace `couchbeam_changes:stream` and `couchbeam_changes:fetch`
  functions by `couchbeam_changes/follow` and `couchbeam_changes:follow_once`.
- breaking change: new attachment API
- new: JSX a pure erlang JSON encoder/decoder is now the default. Jiffy
  can be set at the compilation by defining `WITH_JIFFY` in the Erlang
options.
- removed mochiweb dependency.


version 0.7.0 / 2011-07-05
--------------------------

This release contains backwards incompatible changes.

- New and more efficient couchbeam_changes API, we now parse json stream
  instead of the try catch steps we used before.
- New and more efficient couchbeam_view API. we now parse json stream
  instead of getting all results. New couchbeam_view:stream and
couchbeam_view fetch functions have been added. We also don't use any
more a view record in other functions
- HTTP functions have been moved to couchbeam_httpc modules
- gen_changes behaviour has been updated to use the couchbeam_changes
  API. It's also abble to restart a lost connection for longpoll and
continuous feeds.

Breaking Changes:

- couchbeam:view and couchbeam:all_docs have been deprecated. Old views
  functions using the #view{} record from these functions have been
moved in couchbeam_oldview module.
- couchbeam:wait_changes, couchbeam:wait_changes_once, couchbeam:changes
  functions have been deprecated and are now replaced by
couchbeam_changes:stream and couchbeam_changes:fetch functions.
