ifeq ($(CSC),)
CSC=mcs
endif
ifeq ($(SIL),)
SIL=./sil
endif

all: Sil.exe

extra: all Test.exe Testgen.exe TestSerialization.exe

%.dll: %.scm.sil Sil.exe
	$(SIL) -T $(SIL_FLAGS) -o $* -a Newmoon.dll $<

.PRECIOUS: %.sil
%.sil: %
	(cd ../../; ./newmoon-mz $(NEWMOON_FLAGS) backend/dotnet/$<)

%.exe: %.cs Newmoon.dll
	$(CSC) /debug /t:exe /out:$@ /r:Newmoon.dll $<

%.exe: %.scm.sil Sil.exe
	$(SIL) -T -x $(SIL_FLAGS) -o $* -a Newmoon.dll $<

Newmoon.dll: Newmoon/*.cs
	make -C Newmoon

clean: clean-scheme
	rm -f *.dll *.resource *.exe

clean-scheme:
	rm -f *.sil
