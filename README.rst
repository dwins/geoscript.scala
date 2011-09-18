GeoScript.scala is a Scala library for simple reading, writing, manipulation, and rendering of geospatial data.
It is based on the `GeoTools <http://geotools.org>`_ geospatial library for Java.

Similar GeoTools wrappers are available for Python, Groovy, and JavaScript (using the ringojs platform).
See http://geoscript.org/ for details.

Building
--------

GeoScript.scala is built using `SBT <http://github.com/mharrah/xsbt/>`_.
Follow the SBT `installation instructions, <https://github.com/harrah/xsbt/wiki/Setup` and then::

    sbt test

will build GeoScript.scala and verify it is working properly.  You can then
use::

    sbt console

to launch an interactive console with GeoScript.scala loaded.
Some example scripts can be found in the `examples` subproject.
You can run them using::

   $ sbt
   > project examples
   > run
   ... here SBT prompts you to choose which example to run.

Instead of the ``run`` command, you can use ``run-main`` instead.
It allows you to specify the name of the program to run instead of selecting it interactively, which is more convenient for repeated runs.

Using GeoScript.scala outside of sbt's console is still a work in progress.
