std:
	gnatmake main;gnatmake makebook

fast:
	gnatmake -O2 -gnatN -fomit-frame-pointer -gnatp main;gnatmake -O2 -gnatN -fomit-frame-pointer -gnatp makebook

debug:
	gnatmake -g -gnata main -bargs -E -largs -lgnat ;gnatmake -g -gnata makebook

clean:
	gnatclean -q main;gnatclean -q makebook;\rm -f *~ log* *.pgn *.o

allclean:
	gnatclean -q main;gnatclean -q makebook;\rm -f *~ *.log *.pgn *.bin


