CXX ?= clang++
CXXFLAGS = \
	-I$(CURDIR)/include \
	-fno-rtti \
	-fno-exceptions \
	-Wall \
	-Wextra \
	-Wformat \
	-O3

BIN = glsl-parser

SOURCES = \
	lib/ast.cpp \
	lib/lexer.cpp \
	lib/parser.cpp \
	lib/util.cpp \
	main.cpp

OBJECTS = $(SOURCES:.cpp=.o)

all: $(BIN)

$(BIN): $(OBJECTS)
	$(CXX) $(OBJECTS) -o $@

.cpp.o:
	$(CXX) -MD -c $(CXXFLAGS) $< -o $@
	@cp $*.d $*.P; \
		sed -e 's/#.*//' -e 's/^[^:]*: *//' -e 's/ *\\$$//' \
			-e '/^$$/ d' -e 's/$$/ :/' < $*.d >> $*.P; \
		rm -f $*.d

clean:
	rm -f $(OBJECTS) $(OBJECTS:.o=.P)
	rm -f $(BIN)

test: $(BIN)
	@python3 ./test.py

-include *.P
