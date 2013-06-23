GeoScript.scala is a Scala library for simple reading, writing, manipulation, and rendering of geospatial data.
It is based on the `GeoTools <http://geotools.org>`_ geospatial library for Java.

Similar GeoTools wrappers are available for Python, Groovy, and JavaScript (using the ringojs platform).
See http://geoscript.org/ for details.

Building
--------

GeoScript.scala is built using `SBT <http://scala-sbt.org/>`_.
Follow the SBT `installation instructions, <http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html>`_ and then::

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

There is a screencast about building and running unit tests with SBT available at http://vimeo.com/68050280 .

License information
-------------------
GeoScript.scala is availble under the MIT license.
See the LICENSE file in this repository for details.
