This is the FusionAtlas package.

## Installation ##
* You can download a [snapshot of the current package](https://bitbucket.org/scottmorrison/fusionatlas-package/get/tip.zip).
* You can obtain a read-only, but updateable, version of the repository by installing [Mercurial](https://www.mercurial-scm.org/downloads), then running the command `hg clone https://bitbucket.org/scottmorrison/fusionatlas-package` on the command line. The command `hg pull && hg update` (run inside the `fusionatlas-package` directory) will update to the latest version.
* You can obtain an editable version of the repository by creating an account on BitBucket, emailing Scott Morrison <scott@tqft.net> for permission to commit, and then cloning a copying of the repository with the command
`hg clone https://<username>@bitbucket.org/scottmorrison/fusionatlas-package`, replacing <username> as appropriate.

## Prerequisites ##
* You will need a copy of Java installed (we recommend [JDK at least version 1.8](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html)).
* When you first run the FusionAtlas, it will need to compile many of the Scala libraries used. This can take some time. If you run into trouble, try running the command `./sbt package` in the `fusionatlas-package/scala/` directory.
* You will need a copy of GAP installed in order to do some computations (especially those involving modular data). For now we expect to find a GAP executable at `~/gap/gap`; we're working on making this more flexible.