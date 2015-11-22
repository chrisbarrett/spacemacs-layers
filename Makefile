cwd = $(shell pwd)
dot-spacemacs = $(cwd)/.spacemacs

.PHONY: all submodules

all : ~/.spacemacs submodules

~/.spacemacs :
	ln -s $(dot-spacemacs) $@

submodules :
	git submodule update --init --recursive
