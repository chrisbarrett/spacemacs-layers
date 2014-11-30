cwd = $(shell pwd)
dot-spacemacs = $(cwd)/.spacemacs 

.PHONY: all

all : ~/.spacemacs

~/.spacemacs :
	ln -s $(dot-spacemacs) $@
