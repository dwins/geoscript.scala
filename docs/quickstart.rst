
QuickStart with GeoScript Scala
================================
GeoScript scala is a collection of wrapped GeoTools objects that provided access to GeoTools functionality but presented in a more scala like manner. 


To build GeoScript.scala we use Scala’s simple build tool (sbt). Sbt is similar to Apache Ivy or Maven except its designed specificity for the Scala development environment. With sbt you can keep track of both scala and java based software libraries. 

Install sbt
-------------
Download sbt from the project’s website. 
http://code.google.com/p/simple-build-tool/
http://code.google.com/p/simple-build-tool/wiki/Setup

At the time of this writing a complied jar is available::
	
	wget http://simple-build-tool.googlecode.com/files/sbt-launch-0.7.1.jar

Create a script called sbt:: 

	echo "java -Xmx512M -jar `dirname $0`/sbt-launch-0.7.1.jar "$@"" >> sbt
	
Make that script executable:: 
	
	chmod u+x sbt

Download GeoScript.scala:: 

	git clone git://github.com/dwins/geoscript.scala.git
	
Start sbt::

	cd geoscipt.scala
	sbt

Check out the decencies:: 
	
	update
	
Explore sbt:: 

	actions

For more information check out the sbt Google project page. 




