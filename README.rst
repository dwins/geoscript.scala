GeoScript.scala is a Scala library for simple reading, writing, manipulation,
and rendering of geospatial data.  It is based on the `GeoTools
<http://geotools.org>`_ geospatial library for Java.

Similar GeoTools wrappers are available for Python, Groovy, and JavaScript (using the
narwhal platform).  See http://geoscript.org/ for details.

Building
--------

GeoScript.scala is built using `sbt
<http://simple-build-tool.googlecode.com/>`_.  Follow the sbt installation
instructions, and then::

    sbt update test

will build GeoScript.scala and verify it is working properly.  You can then
use::

    sbt console

to launch an interactive console with GeoScript.scala loaded.  Some example
scripts can be found in the `examples` subproject.

Using GeoScript.scala outside of sbt's console is still a work in progress.
