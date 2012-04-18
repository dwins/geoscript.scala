.. highlight:: sh

Quick Start
===========

GeoScript.scala provides a streamlined Scala API for geospatial data access and
operations.  Internally, it uses the `GeoTools library
<http://geotools.org/>`_.

In this quickstart, we will cover the following:

#. Build GeoScript.scala from sources
#. Use GeoScript in a Scala interpreter


Install sbt
-----------

Download ``sbt`` from the project’s `website
<http://code.google.com/p/simple-build-tool/downloads/list>`_.


At the time of this writing a complied jar is available::
	
	$ wget http://simple-build-tool.googlecode.com/files/sbt-launch-0.7.1.jar

Create a script called ``sbt``:: 

	$ echo "java -Xmx512M -jar `dirname $0`/sbt-launch-0.7.1.jar "$@"" >> sbt
	
Make that script executable:: 
	
	$ chmod u+x sbt


Set Up GeoScript Scala
----------------------

Download GeoScript.scala:: 

	$ git clone git://github.com/dwins/geoscript.scala.git
	
Start ``sbt``::

	$ cd geoscript.scala/
	$ sbt

.. highlight:: none

Fetch the dependencies, build GeoScript, and run the test suite:: 
	
	> update
	> compile
	> test
	
Explore ``sbt``:: 

	> actions

Start the Scala command line interpreter:: 

	> console
	scala> import org.geoscript._
	scala> import GeoScript._

For more information check out the ``sbt`` `Google project page
<http://simple-build-tool.googlecode.com/>`_. 
