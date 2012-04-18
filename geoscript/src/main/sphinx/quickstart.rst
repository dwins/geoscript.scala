.. highlight:: sh

Quick Start
===========

This guide will help you get up and running with GeoScript Scala.

Java Virtual Machine
--------------------

GeoScript Scala requires the Java Virtual Machine in order to operate.  Most systems have Java preinstalled; you can verify this with the command::

    $ java -version
    java version "1.5.0_20"
    Java(TM) 2 Runtime Environment, Standard Edition (build 1.5.0_20-b02-315)
    Java HotSpot(TM) Client VM (build 1.5.0_20-141, mixed mode, sharing)

If your Java version is ``1.5`` or greater, you are ready to install GeoScript
Scala.  Otherwise, we recommend installing Sun's JVM, available from `the Sun
website <http://java.sun.com/javase/downloads/widget/jdk6.jsp>`_.


Installing GeoScript
--------------------

You can download an archive of the latest GeoScript Scala release at the
`GitHub download page<http://github.com/dwins/geoscript.scala/downloads>`_.  Unpack the archive to your filesystem::

    $ tar xvzf geoscript-0.1.tar.gz

After unpacking, you can run the GeoScript interpreter by specifying the path::

    $ geoscript-0.1/bin/geoscript-scala
    Welcome to Scala version 2.7.7.final (HotSpot(TM) Client VM, Java 1.5.0_20-141)
    Type in expressions to have them evaluated.
    Type :help for more information.

    scala>

Adding to the PATH
------------------

In order to avoid needing to provide the path every time you want to use
GeoScript, you can add GeoScript to your shell's PATH variable.  On Mac OSX,
Linux, and other Unixy systems, you can do this::

    $ echo 'export PATH=/path/to/geoscript/:$PATH' >> ~/.profile
    $ . ~/.profile # apply changes, otherwise they won't apply until next login

On Windows, you can do this by editing your PATH environment variable through
the Properties dialog for My Computer.

.. Other Options
   -------------
   
   You can also :doc:`build GeoScript Scala from sources<building>` or
   :doc:`include<including>` it in a managed project.
