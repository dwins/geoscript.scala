.. _tutorial:

GeoScript Tutorial
====================

In this tutorial we are going download and process census blocks from New York City.  
We are going to use the GeoScript.scala library. 

This tutorial highlights the features of the GeoScript library by examining how to 
process data from the US Census Department.

The goals for this project are

# Install geoscript
  #. Setup sbt
  #. Check out geoscript
  #. Install decencies and publish-local

# Create new sbt project
  #. Create folder CensusMap
  #. Setup and configure sbt
  #. Add decencies and install 

# Download census data
  #. Download census blocks from Bytes of the Big Apple
  #. Download tabular data from Fact Finder. 

	

Install GeoScript
-----------------
This tutorial requires you to have sbt installed on your system. Check out a fresh copy of GeoScript.::

	$ git clone git://github.com/dwins/geoscript.scala.git 

Update to install all decencies::

	$ cd geoscript.scala
	$ sbt 
	> update  

Publish GeoScript locally::

	> publish-local

Create a new  sbt project.::

	$ mkdir census
	$ cd census
	$ sbt 


 
