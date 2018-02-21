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

OBJS = parser.o  \
       tokens.o  \
       codegen.o \
       yaml.o

LLVMPATH=/Users/yixin/llvm
LLVMCONFIG =$(LLVMPATH)/bin/llvm-config

CPPFLAGS=-std=c++11 -Wno-deprecated-register `$(LLVMCONFIG) --cppflags`
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
	g++ -o $@ $(OBJS) $(LIBS) $(LDFLAGS) $(CPPFLAGS) $(INCLUDES)

test: parser test1.ek
	./parser test1.ek -emit-ast

