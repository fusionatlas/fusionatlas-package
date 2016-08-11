#!/bin/bash
# download things we need
#SCALAVERSION=scala-2.8.0.final
#SCALAVERSION=scala-2.8.1.RC1
SCALAVERSION=scala-2.9.0.final
SCALATGZ=$SCALAVERSION.tgz

WORKERS="$1"
if [ -n "$2" ]; then
    export JAVA_HOME=$2
fi

mkdir -p fusionatlas
cd fusionatlas

echo Looking for $SCALAVERSION. > prepare.log
if [ ! -f $SCALATGZ ]
then
	echo Downloading $SCALAVERSION... >> prepare.log
	curl http://www.scala-lang.org/downloads/distrib/files/$SCALATGZ > $SCALATGZ
	echo Finished downloading. >> prepare.log
fi
# unpack scala
tar zxvf $SCALATGZ

if [ -f org.fusionatlas.jar ] && [ ! -s org.fusionatlas.jar ]
then
	rm org.fusionatlas.jar
fi
echo Downloading org.fusionatlas.jar. >> prepare.log
curl -z org.fusionatlas.jar -Ro org.fusionatlas.jar http://tqft.net/svn/fusionatlas/package/scala/target/org.fusionatlas.jar
echo Finished downloading >> prepare.log
unzip -o -d fusionatlas org.fusionatlas.jar

# kill any running QueueWorker
# echo Killing errant processes... >> prepare.log
# kill -9 `ps -Af | grep java | grep "QueueWorker" | grep -v grep | awk '{print $2}'` >> prepare.log 2>&1

# and run QueueWorker
echo Launching QueueWorker >> prepare.log
CLASSPATH="fusionatlas/:"
for file in `ls fusionatlas/jars/*.jar`; do
	CLASSPATH=$CLASSPATH"$file:"
done
echo $CLASSPATH >> prepare.log
export JAVA_OPTS=-Xmx1400m
./$SCALAVERSION/bin/scala -classpath $CLASSPATH org.fusionatlas.enumerators.QueueWorker $WORKERS > QueueWorker.log 2>&1

