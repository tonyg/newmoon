ifeq ($(CSC),)
CSC=mcs
ILASM=ilasm
AL=al
endif

all: Newmoon.dll

clean:
	rm -f Newmoon.dll Newmoon.netmodule _temp.dll Lowlevel.obj

%.obj: Newmoon/%.il
	$(ILASM) -o $@ -j $<

_temp.dll: Lowlevel.obj
	$(AL) -o $@ -d $<

Newmoon.netmodule: Newmoon/*.cs _temp.dll
	$(CSC) /debug /t:module /out:$@ Newmoon/*.cs /r:_temp.dll

Newmoon.dll: Newmoon.netmodule Lowlevel.obj
	$(AL) -o $@ -d $^
