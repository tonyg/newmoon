ifeq ($(CSC),)
CSC=mcs
endif

../Newmoon.dll: *.cs
	$(CSC) /debug /t:library /out:$@ $^
