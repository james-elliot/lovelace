std:
	gnatmake -gnat2022 -mcmodel=large main;gnatmake -gnat2022 makebook

fast:
	gnatmake -gnat2022 -mcmodel=large -O3 -flto=auto -gnatn -fomit-frame-pointer -gnatp main;gnatmake -gnat2022 -O2 -gnatN -fomit-frame-pointer -gnatp makebook

debug:
	gnatmake -mcmodel=large -gnat2022 -g -gnata main -bargs -E -largs -lgnat ;gnatmake -gnat2022 -g -gnata makebook

clean:
	gnatclean -q main;gnatclean -q makebook;\rm -f *~ *.log *.pgn *.o

allclean:
	gnatclean -q main;gnatclean -q makebook;\rm -f *~ *.log *.pgn *.o *.bin


