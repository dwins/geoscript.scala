
QuickStart with GeoScript Scala
================================


Install sbt
-------------
Download sbt from the project’s `website http://code.google.com/p/simple-build-tool/`_ 


At the time of this writing a complied jar is available::
	
	wget http://simple-build-tool.googlecode.com/files/sbt-launch-0.7.1.jar

Create a script called sbt:: 

	echo "java -Xmx512M -jar `dirname $0`/sbt-launch-0.7.1.jar "$@"" >> sbt
	
Make that script executable:: 
	
	chmod u+x sbt

Download GeoScript.scala:: 

	git clone git://github.com/dwins/geoscript.scala.git
	
Start sbt::

	cd geoscipt.scala/
	sbt

Check out the decencies:: 
	
	update
	
Explore sbt:: 

	actions

For more information check out the sbt Google project page. 




