# flymake requires the following target

check-syntax:
	g++ -o nul `libpng14-config --cflags --libs` `freetype-config --cflags --libs` -Wall -S $(CHK_SOURCES)

aa:fttut.cpp
	g++ -o aa `libpng14-config --cflags --libs` `freetype-config --cflags --libs` fttut.cpp

so:fttut.cpp
	g++ -c `libpng14-config --cflags --libs` `freetype-config --cflags` fttut.cpp
#	g++ -dylib -shared `libpng14-config --ldflags` `freetype-config --libs` fttut.o -o fttut.so
	g++ -dynamiclib `libpng14-config --ldflags` `freetype-config --libs` fttut.o -o libfttut.dylib