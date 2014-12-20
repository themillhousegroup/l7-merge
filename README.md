l7-merge
============================

Given a directory of XML files exported from a layer7 network appliance,
computes the minimal set of changes required to make it functionally 
identical to a second directory of "newer" exported files.

This is useful for managing these files with a conventional source control system, as
the "built-in" system implemented within the L7 results in very large deltas.

#### Usage

You'll need to have Java installed in your `PATH`.

Run it with 

 - `[PATH_TO_L7-MERGE]/bin/l7-merge` (UNIX-like OSes); or
 - `[PATH_TO_L7-MERGE]\bin\l7-merge.bat` (Windows)
 
and it will tell you about the available commands:

```
compare-one [file1] [file2] to compare the contents of two files

merge-one <file1> <file2> [merge-options] to merge the contents of file2 into file1

Where [merge-options] are:
--force                 Merge even if the files seem 'too different'
--only-structural       Retain references to old GUIDs - i.e. changes are structural to this file
--version-aware         Inspect for version numbers and use those to determine the older/newer file
--retain-old-versions   Keep the 'version' numbers from the 'older' file (minimises diffs)

visualise [directory] to visualise the contents of a directory
```

##### Typical invocation:
Say you've got a recently-exported Layer7 XML file called `export/service-changed.xml`.

Your Git repository of Layer7 configuration files is at `../git/src`.

To merge back the minimal differences between your exported file and Git:

```
[PATH_TO_L7-MERGE]/bin/l7-merge merge-one ../git/src/service-changed.xml export/service-changed.xml --force --only-structural --retain-old-versions
```

Now a `git diff` in your Git repo should show the minimal changes.


#### Developing

l7-merge is written in Scala and builds with SBT. Test with `sbt test`.


Build the runnable app with `sbt stage`.