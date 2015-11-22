emacs_d = ~/.emacs.d
cwd = $(shell pwd)
dot-spacemacs = $(cwd)/.spacemacs

.PHONY: all submodules install reinstall

all : install ~/.spacemacs submodules

install :
	emacs --batch -l $(emacs_d)/init.el

reinstall :
	rm -r $(emacs_d)/elpa
	emacs --batch -l $(emacs_d)/init.el

submodules :
	git submodule update --init --recursive

~/.spacemacs :
	ln -s $(dot-spacemacs) $@
