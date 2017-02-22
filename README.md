tsani's Pushbullet tools
========================

[![Hackage](https://img.shields.io/hackage/v/tpb.svg)]()

This repository contains several (*ahem* two *ahem*) applications for
interacting with the Pushbullet API: `tpb` and `pb-notify`.

The former provides a command-line interface to some of the Pushbullet API
endpoints. The latter listens to the Pushbullet realtime event stream and
dispatches notifications (libnotify).

Read more about these applications in [my blog post][post].

### Features

#### tpb

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

tpb comes with a helper script `sms` that provides a more intuitive command
syntax; just type `sms to jacob "hello world"` to send a text message!

#### pb-notify

  * send libnotify notifications when SMS are received by any of your
    Pushbullet-connected devices.

[post]: https://jerrington.me/posts/2017-02-20-sms-command-line.html
