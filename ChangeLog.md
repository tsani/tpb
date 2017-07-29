# Revision history for tpb

## 0.3.0.0  -- 2017-07-29

* implement clipboard integration in pb-notify via a built-in HTTP server.
* add script pbclip for interacting with the pushbullet clipboard via
  pb-notify.
* tpb: add `devices create` and `devices remove` commands
* tpb: support pushbullet-types 0.3
* bump dependency on servant-pushbullet-client to 0.3

## 0.2.0.0  -- 2017-04-30

* Partially fix connectivity bugs in pb-notify.
* Mirror pushes in pb-notify.

## 0.1.1.1  -- 2017-02-20

* add `app_name` libnotify hint in pb-notify, so applications processing
  notifications sent by pb-notify can do special logic.

## 0.1.1.0  -- 2017-02-20

* remove unused module.
* declare all modules in `other-modules` so cabal includes them in source
  distributions.

## 0.1.0.0  -- 2017-02-20

This version releases the `tpb` and `pb-notify` programs as well as some
accompanying scripts.

### tpb features

* multiple output formats:
  * for standalone usage, nice human-readable formatting; and
  * for usage in scripts, tabular output as JSON separated values.
* list devices.
* list SMS threads on a device.
* list contents of an SMS thread on a device, either:
  * by thread ID; or
  * by fuzzy-matching the recipient's name to find the thread ID.
* send SMS, either :
  * by giving an explicit phone number; or
  * by fuzzy-matching the recipient's name to find their phone number.

tpb requires no configuration if you fill out all the command line options, but
some options that are used everywhere (like the Pushbullet API key and the
device ID, assuming you use only one device) can be specified via environment
variables.

### pb-notify features

* API key configuration via environment variables.
* Any SMS received on any Pushbullet device associated to the API key will be
  shown as a desktop notification via libnotify.
