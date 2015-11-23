emacs = emacs --batch
emacs_d = ~/.emacs.d
cwd = $(shell pwd)
package_d = $(emacs_d)/elpa
dot_spacemacs = $(cwd)/.spacemacs
init_el = $(emacs_d)/init.el

.PHONY: all submodules install reinstall

all : install ~/.spacemacs submodules

install :
	$(emacs) -l $(init_el)

reinstall :
	[ -d $(package_d) ] && rm -r $(package_d)
	$(emacs) -l $(init_el)

submodules :
	git submodule update --init --recursive

~/.spacemacs :
	ln -s $(dot_spacemacs) $@
