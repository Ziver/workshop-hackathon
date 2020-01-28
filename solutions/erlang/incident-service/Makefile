ERL=/usr/bin/erl
ERLC=/usr/bin/erlc
LIBDIR=../lib
SRCDIR=src
BEAMDIR=./ebin

all:
	mkdir -p $(BEAMDIR)
	cp $(LIBDIR)/yaws/ebin/*.beam $(BEAMDIR)
	$(ERLC) -o $(BEAMDIR) $(SRCDIR)/*.erl

dep-ywas:
	cd $(LIBDIR)/yaws
	autoreconf -fi
	./configure
	make

clean:
	rm -rf $(BEAMDIR)
	rm -rf erl_crush.dump

run: all
	cd $(BEAMDIR)
	#$(ERL) -s ybed start -s init stop
	$(ERL) -noshell -s hello start -s init stop
