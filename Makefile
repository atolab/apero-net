.PHONY: lib example all clean test doc

BUILD_LIB=dune build	
BUILD_EXAMPLE=\
	dune build example/echo/echo.exe \
    example/echoes/echo.exe  \
	example/secho/secho.exe  \

CLEAN= dune clean
TEST=dune runtest -j1 --no-buffer
DOC=dune build --dev @doc
INSTALL=dune install


all:	
	make lib	
	make example 


lib:
	${BUILD_LIB}		

example:	
	${BUILD_EXAMPLE}

	
test:
	${TEST}

doc:
	${DOC}

install:
	${INSTALL}

clean:
	${CLEAN}
