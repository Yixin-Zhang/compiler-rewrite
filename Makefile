all: parser

OBJS = parser.o  \
       tokens.o  \
       codegen.o \
       yaml.o \
       main.o \
       corefn.o

LLVMPATH=~/llvm
LLVMBIN=$(LLVMPATH)/bin
LLVMCONFIG =$(LLVMPATH)/bin/llvm-config

CPPFLAGS=-std=c++11 -g -Wno-deprecated-register `$(LLVMCONFIG) --cppflags`
LDFLAGS=-ll `$(LLVMCONFIG) --ldflags` -lpthread -ldl -lz -lncurses -rdynamic
LIBS = `$(LLVMCONFIG) --libs`
INCLUDES=-I$(LLVMPATH)/include

clean:
	rm -rf parser parser.cpp parser.hpp tokens.cpp $(OBJS)

parser.cpp: parser.y
	bison -d -o $@ $^
	
parser.hpp: parser.cpp

tokens.cpp: tokens.l parser.hpp
	flex -o $@ $^

%.o: %.cpp
	g++ -c -o $@ $< $(LDFLAGS) $(CPPFLAGS) $(INCLUDES) $(CPPFLAGS)

parser: $(OBJS)
	$(LLVMBIN)/clang++ -o $@ $(OBJS) $(LIBS) $(LDFLAGS) $(CPPFLAGS) $(INCLUDES)

test: parser test1.ek
	./parser test1.ek -emit-ast

