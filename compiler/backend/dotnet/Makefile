ifeq ($(CSC),)
CSC=mcs
ILASM=ilasm
AL=al
DIS=monodis
endif

all: Newmoon.dll

clean:
	rm -f Newmoon.dll NewmoonHighlevel.dll NewmoonLowlevel.dll

NewmoonLowlevel.dll: Newmoon/Lowlevel.il
	$(ILASM) /dll /out:$@ $<

NewmoonHighlevel.dll: Newmoon/*.cs NewmoonLowlevel.dll
	$(CSC) /debug /t:library /out:$@ Newmoon/*.cs /r:NewmoonLowlevel.dll

Newmoon.dll: NewmoonHighlevel.dll NewmoonLowlevel.dll
	$(DIS) NewmoonHighlevel.dll > Newmoon.il
	$(DIS) NewmoonLowlevel.dll >> Newmoon.il
	cat Newmoon.il | sed -e 's/NewmoonHighlevel/Newmoon/g' -e 's/NewmoonLowlevel/Newmoon/g' > Newmoon.tmp.il
	mv Newmoon.tmp.il Newmoon.il
	echo '.assembly '"'"'Newmoon'"'"' {} .module Newmoon' >> Newmoon.il
	$(ILASM) /dll /out:$@ Newmoon.il
	rm -f Newmoon.il Newmoon.tmp.il
