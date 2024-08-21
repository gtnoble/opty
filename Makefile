INSTALL_DIR = /home/gtnoble/.local/bin
CSC = csc
CSC_FLAGS = -O3 -debug o -verbose
CSC_SO_FLAGS = $(CSC_FLAGS) -s
CSC_BIN_FLAGS = $(CSC_FLAGS) -static

.PHONY: all

all: opty gen-guesses

opty: main.scm pso.scm 
	$(CSC) $(CSC_BIN_FLAGS) -o $@ pso.scm $<

gen-guesses: gen-guesses.scm
	$(CSC) $(CSC_BIN_FLAGS) -o $@ $<

.PHONY: install

install: opty gen-guesses
	cp opty $(INSTALL_DIR)
	cp gen-guesses $(INSTALL_DIR)

clean:
	rm -f opty *.so *.test PROFILE.* *.types *.o *.link

%.so: %.scm
	$(CSC) $(CSC_SO_FLAGS) $<

%.test: %.so
	$(CSC) $(CSC_BIN_FLAGS) -o $@ $<
