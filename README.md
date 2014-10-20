l7-merge
============================

Given a directory of XML files exported from a layer7 network appliance,
computes the minimal set of changes required to make It functionally 
identical to a second directory of "newer" exported files.

This is useful for managing these files with a conventional source control system.

#### Usage

Build the runnable app with `sbt stage`.

Run it with `./target/universal/stage/bin/l7-merge`
and it will tell you about the available commands
