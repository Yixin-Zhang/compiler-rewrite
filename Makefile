all: parser

# OBJS = parser.o  \
#        codegen.o \
#        main.o    \
#        tokens.o  \
#        corefn.o  \
# 	   native.o  \
# LLVMCONFIG = llvm-config
# CPPFLAGS = `$(LLVMCONFIG) --cppflags` -std=c++11
# LDFLAGS = `$(LLVMCONFIG) --ldflags` -lpthread -ldl -lz -lncurses -rdynamic
# LIBS = `$(LLVMCONFIG) --libs`

OBJS = parser.o  \
       tokens.o  \

CPPFLAGS=-std=c++11 -Wno-deprecated-register
LDFLAGS=-ll
INCLUDES=-I/Users/yixin/llvm/include
clean:
	rm -rf parser.cpp parser.hpp parser tokens.cpp $(OBJS)

parser.cpp: parser.y
	bison -d -o $@ $^
	
parser.hpp: parser.cpp

tokens.cpp: tokens.l parser.hpp
	flex -o $@ $^

%.o: %.cpp
	g++ -c -o $@ $< $(LDFLAGS) $(CPPFLAGS) $(INCLUDES) $(CPPFLAGS)

parser: $(OBJS)
	g++ -o $@ $(OBJS) $(LIBS) $(LDFLAGS) $(CPPFLAGS) $(INCLUDES)

test: parser example.txt
	./parser example.txt -emit_ast
