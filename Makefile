COMPILE = guild compile
PDF = texi2pdf 
HTML = makeinfo 

DOC_SRC = doc/api.texi

GUILE_SRC = 	irc.scm \
		message.scm \
		tagged-hook.scm \
		handlers.scm \
		error-code.scm \
		channels.scm

all: doc

doc: makedir pdf html

makedir:
	mkdir build/

pdf:	$(DOC_SRC) 
	cd build/; $(PDF) --build=clean -q ../$(DOC_SRC) 

html:	$(DOC_SRC)
	$(HTML) --html $(DOC_SRC) -o build/html/

clean:
	rm -rf build/*
