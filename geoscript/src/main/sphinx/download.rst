.. highlight:: scala

Getting GeoScript.scala
=======================

This guide will help you get up and running with GeoScript Scala.
Unlike some other GeoScript variants, the Scala variant does not ship a pre-packaged bundle with all dependencies in one download.
Instead, we recommend using one of the build tools listed below for trying out GeoScript Scala; both provide an interactive interpreter along with the ability to compile and run sources.

SBT
===

SBT is a popular build tool for Scala projects.
SBT build configurations can be written in pure Scala, or in "build.sbt" files which are briefer but less customizable.

Getting SBT
-----------
Instructions for getting SBT are available at http://scala-sbt.org/

Configuration
-------------

A simple build configuration using GeoScript Scala follows:

.. literalinclude:: sample_sbt_build.sbt
    :language: scala

Commands
--------

Some common commands that may be useful when working with GeoScript in SBT.

* ``sbt console`` launches a Scala :abbr:`REPL<Read-eval-print-loop>` or interactive shell with the GeoScript library loaded.

* ``sbt run`` finds an executable object in your project's sources and executes it.
  Command-line arguments may be provided.
  If you have multiple executable objects this command will prompt to let you choose which to run; for repeated runs, it's often more convenient to use ``sbt run-main qualified.name.of.Executable``.

* ``sbt test`` runs unit tests for your project. 
  You should load `Specs2 <http://etorreborre.github.com/specs2/>`_ or `ScalaTest <http://www.scalatest.org/>`_ to write your tests.
  `JUnit <http://junit.org/>`_ is also supported through the `junit-interface <https://github.com/szeiger/junit-interface>`_ plugin.

* Any SBT command can be prefixed with a tilde (``~``) to be re-run automatically when source files are modified.
  For example, if you leave ``sbt ~test`` running while you edit your sources, you'll be notified of test failures immediately after making the changes that introduce them.

* If you run ``sbt`` with no arguments it will enter its own interactive console, which provides tab-completion.
  This is also useful to keep the JVM "warmed-up" between running different SBT commands.

* For more information, see the SBT wiki at https://github.com/harrah/xsbt/wiki/ .

Maven
=====

Maven is a popular build tool for Java projects and can also be used with Scala.

Configuration
-------------

You can download a :download:`sample POM <sample_maven_pom.xml>` for a GeoScript Maven project.
This configuration uses the `Maven Scala plugin <http://scala-tools.org/mvnsites/maven-scala-plugin/>`_ as well as loading the GeoScript library and its dependencies.

Commands
--------

The following are some common commands that may be useful when working with GeoScript in Maven.

* ``mvn scala:cc`` watches the source directory and re-runs the compiler when files change; handy to see compile errors immediately after introducing them.

* ``mvn scala:console`` launches an interactive Scala interpreter with your project and all dependencies loaded.

* See http://maven.apache.org/ and http://scala-tools.org/mvnsites/maven-scala-plugin/ for more information.

Others
======

Using GeoScript Scala is, in theory, possible with any build tool that supports the Scala compiler and Maven repositories for fetching dependencies.
If you want to do so, here is the information.

Repositories
------------

GeoScript Scala itself is published in the OpenGeo maven repository at http://repo.opengeo.org/

It requires dependencies that are hosted in the OSGeo Maven repository at http://download.osgeo.org/webdav/geotools/

Artifacts
---------

GeoScript Scala publishes two artifacts:

* ``org.geoscript:geoscript_2.9.1:geocss`` implements support for a CSS-like language that is translated to GeoTools (and therefore GeoScript) Style objects.
* ``org.geoscript.geoscript_2.9.1:geoscript`` is the GeoScript library itself.
