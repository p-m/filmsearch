all: filmsearch imdb2fs make-mkv

%: %.lisp Makefile
	@echo -e "(load \"$*.lisp\")\n(sb-ext:save-lisp-and-die" \
		"\"$*\" :toplevel #'main :executable t)" | sbcl
