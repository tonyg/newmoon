ifeq ($(CSC),)
CSC=mcs
ILASM=ilasm
AL=al
DIS=monodis
endif

all: Newmoon.dll

clean:
	rm -f Newmoon.dll

Newmoon.dll: Newmoon/*.cs Newmoon/Lowlevel.il
	$(CSC) /debug /t:library /out:$@ Newmoon/*.cs
#	$(DIS) Newmoon.dll > Newmoon.il
#	cat Newmoon/Lowlevel.il >> Newmoon.il
#	$(ILASM) /dll /out:$@ Newmoon.il
	rm -f Newmoon.il
