INSTALL_DIR = ~/.local/bin
CSC = csc
CSC_FLAGS = -d3 -debug o -vv
CSC_SO_FLAGS = $(CSC_FLAGS) -s
CSC_BIN_FLAGS = $(CSC_FLAGS) -static

.PHONY: all

all: opty gen-particles

opty: main.scm pso.scm 
	$(CSC) $(CSC_BIN_FLAGS) $^ -o $@ 

gen-particles: gen-particles.scm quasi-random.scm 
	$(CSC) $(CSC_BIN_FLAGS) $^ -L -lgsl -o $@ 

quasi-random.so: quasi-random.scm
	$(CSC) $(CSC_SO_FLAGS) $< -L -lgsl  -o $@ 

.PHONY: install

install: opty gen-particles
	cp opty $(INSTALL_DIR)
	cp gen-particles $(INSTALL_DIR)
	cp jsenv $(INSTALL_DIR)

clean:
	rm -f opty gen-particles *.so *.test PROFILE.* *.types *.o *.link

%.so: %.scm
	$(CSC) $(CSC_SO_FLAGS) $<

%.test: %.so
	$(CSC) $(CSC_BIN_FLAGS) -o $@ $<
