cd firrtl
git pull
sbt assembly
cp utils/bin/firrtl.jar ../lib
mkdir -p ../chisel3/lib
cp utils/bin/firrtl.jar ../chisel3/lib
mkdir -p ../firrtl-interpreter/lib
cp utils/bin/firrtl.jar ../firrtl-interpreter/lib
mkdir -p ../treadle/lib
cp utils/bin/firrtl.jar ../treadle/lib
cd ../firrtl-interpreter
git pull
sbt package
cp target/scala-2.11/firrtl-interpreter_2.11-1.2-SNAPSHOT.jar ../lib
cp target/scala-2.11/firrtl-interpreter_2.11-1.2-SNAPSHOT.jar ../chisel3/lib
cd ../treadle
git pull
sbt package
cp target/scala-2.11/treadle_2.11-1.1-SNAPSHOT.jar ../lib
cp target/scala-2.11/treadle_2.11-1.1-SNAPSHOT.jar ../chisel3/lib
cd ../chisel3
git pull
sbt package
cp target/scala-2.11/chisel3_2.11-3.1-SNAPSHOT.jar ../lib

