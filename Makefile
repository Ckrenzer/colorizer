.PHONY: build

cl_implementation ?= sbcl

# needs Quicklisp to be installed!
build:
	$(cl_implementation) --load app.lisp
