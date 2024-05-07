std:
	gnatmake  -mcmodel=large  main;gnatmake  -mcmodel=large makebook

fast:
	gnatmake   -mcmodel=large -O3 -flto=auto -gnatn -fomit-frame-pointer -gnatp main;gnatmake  -O2 -gnatN -fomit-frame-pointer -gnatp makebook

debug:
	gnatmake -mcmodel=large  -g -gnata main -bargs -E -largs -lgnat ;gnatmake  -g -gnata makebook

clean:
	gnatclean -q main;gnatclean -q makebook;\rm -f *~ *.log *.pgn *.o

allclean:
	gnatclean -q main;gnatclean -q makebook;\rm -f *~ *.log *.pgn *.o *.bin


