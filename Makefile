clean:
	@rm -f init.elc bwoah.el bwoah.elc

compile: init.el bwoah.org clean
	@emacs -Q --batch -l 'lisp/compile.el'