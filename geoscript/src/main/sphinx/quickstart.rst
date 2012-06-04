Quick Start
===========

This guide will help you get up and running with GeoScript Scala.
We assume you are using `SBT <http://scala-sbt.org/>`_ to build your project.
See :doc:`download` for alternative setup information.

Set up a project
----------------

After installing SBT, you'll need to setup your project.
An SBT project is just a directory that contains an SBT build configuration (and whatever source code you have, but we'll get to that shortly.)
Usually, it's a good idea to start a project by creating a directory, then changing directories into it:

.. code-block: sh

    $ mkdir my-first-geoscript
    $ cd my-first-geoscript

Next, create a ``built.sbt`` file containing the configuration for your project.
A minimal configuration follows:

.. literalinclude:: sample_sbt_build.sbt
   :language: scala

This file belongs in the root of the project directory; that is, if your project directory is :file:`hello-geoscript` then the build configuration should be stored in :file:`hello-geoscript/build.sbt`.

Try it 
------

Now you can verify that GeoScript is available by using the interactive Scala interpreter, or REPL (short for "Read Eval Print Loop.")  
To do so, use sbt:

.. code-block:: sh

    $ sbt console
    [info] Loading project definition from /home/dwins/hello-geoscript/project
    [info] Set current project to default-79d942 (in build file:/home/dwins/hello-geoscript/)
    [info] Starting scala interpreter...
    [info] 
    Welcome to Scala version 2.9.1.final (Java HotSpot(TM) Server VM, Java 1.6.0_30).
    Type in expressions to have them evaluated.
    Type :help for more information.

    scala> 

With the console, you can type in Scala code and see the results immediately.
Let's try it out by buffering a simple point geometry:

.. code-block:: scala

   scala> import org.geoscript._
   import org.geoscript._

   scala> geometry.point(1,2)
   res0: org.geoscript.geometry.package.Point = POINT (1 2)

   scala> res0.buffer(0.2)
   res1: com.vividsolutions.jts.geom.Geometry = POLYGON ((1.2 2, 1.196157056080646 1.9609819355967744, 1.1847759065022574 1.923463313526982, 1.1662939224605091 1.8888859533960796, 1.1414213562373094 1.8585786437626906, 1.1111140466039204 1.8337060775394909, 1.076536686473018 1.8152240934977426, 1.0390180644032256 1.803842943919354, 1 1.8, 0.9609819355967744 1.803842943919354, 0.9234633135269821 1.8152240934977426, 0.8888859533960796 1.8337060775394909, 0.8585786437626906 1.8585786437626906, 0.8337060775394909 1.8888859533960796, 0.8152240934977426 1.923463313526982, 0.8038429439193538 1.9609819355967744, 0.8 2, 0.803842943919354 2.039018064403226, 0.8152240934977427 2.0765366864730184, 0.8337060775394911 2.111114046603921, 0.8585786437626908 2.1414213562373097, 0.8888859533960798 2.1662939...
   scala> res1.getArea
   res2: Double = 0.12485780609032211

Coding
------

While the REPL is handy for experimenting, often when writing code with GeoScript Scala it's useful to store the code in a file for later reuse.

Let's do the same calculation from above as an executable program.
First, create a source directory.
SBT will look for Scala sources in :file:`src/main/scala/`, so we should create that and any necessary parent directories::

    $ mkdir -p src/main/scala/

Next, create a HelloGeoScript.scala file in that directory.
You can use the App trait from the Scala standard library to easily make an executable.

.. code-block:: scala

   import org.geoscript._
   object HelloGeoScript extends App {
       val p1 = geometry.point(1, 2)
       val p2 = p1.buffer(0.2)
       println("Area of buffered point is: " + p2.getArea)
   }

.. note::

   In compiled code, Scala won't automatically print all calculation results for us.
   We have to explicitly use the ``println`` function to display values that we are interested in.

Now, instead of using SBT's ``console`` command, use ``run`` to run the code::

    $ sbt run
    [info] Loading project definition from /home/dwins/hello-geoscript/project
    [info] Set current project to default-79d942 (in build file:/home/dwins/hello-geoscript/)
    [info] Running HelloGeoScript
    Area of buffered point is: 0.12485780609032211
    [success] Total time: 4s, completed Jun 3, 2012, 9:01:59 PM

What's next
-----------

From here, you can view the `documentation <../learning>`_ to learn more about what GeoScript Scala can do, or the `reference <api>`_ for the GeoScript Scala API.
If you're new to Scala, there's a quick `Scala tutorial <introduction>`_ as well as the community documenation at http://docs.scala-lang.org/ .
