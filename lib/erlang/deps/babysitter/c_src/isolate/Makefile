CFLAGS += -Wall -Wextra -Os
LDFLAGS += -lelf

SOURCES = isolate.cpp configuration.sh privilege.h privilege.cpp help.h

default: isolate

configuration.h:
	sh configuration.sh > configuration.h

isolate: $(SOURCES) configuration.h
	c++ $(CFLAGS) $(LDFLAGS) -o isolate isolate.cpp privilege.cpp
	chmod u+s isolate

clean: cleandoc
	-rm *.o isolate err configuration.h

doc: isolate.tex isolate.bib
	pdflatex isolate.tex > err 2>&1
	bibtex isolate >> err 2>&1
	pdflatex isolate.tex >> err 2>&1
	pdflatex isolate.tex >> err 2>&1
	evince isolate.pdf &

cleandoc:
	-rm err *.log *.aux *.out *.pdf *.bbl *.blg *.brf *.ilf *.ind *.ilg

all: cleandoc clean isolate doc

install: isolate
	install -S -o 0 -g 0 -m a=rx,u+s -s isolate /usr/local/bin/isolate
	install -S -o 0 -g 0 -m 0644 isolate.1 /usr/local/man/man1/isolate.1
	gzip -9 /usr/local/man/man1/isolate.1

