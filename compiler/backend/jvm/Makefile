SOURCES=$(shell find sisc -name '*.java')
BCELJAR=./bcel-5.1.jar
CLASSPATH=.:$(SISC_HOME)/sisc.jar:$(BCELJAR)

all: sisc-newmoon.jar

sisc-newmoon.jar: $(SOURCES)
	-mkdir './=classes'
	javac -classpath $(CLASSPATH) -d './=classes' $(SOURCES)
	jar -cvf $@ -C './=classes' .

clean:
	rm -rf './=classes'
	rm -f sisc-newmoon.jar
