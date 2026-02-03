FROM ubuntu:24.04 AS glibc

RUN apt-get update && \
    apt-get install -y sbcl curl && \
    curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load ./quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(quit)" && \
    rm quicklisp.lisp && \
    echo '#-quicklisp (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))) (when (probe-file quicklisp-init) (load quicklisp-init)))' > $HOME/.sbclrc


FROM alpine:3.23 AS musl

RUN apk update && \
    apk add sbcl curl && \
    curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load ./quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(quit)" && \
    rm quicklisp.lisp && \
    echo '#-quicklisp (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))) (when (probe-file quicklisp-init) (load quicklisp-init)))' > $HOME/.sbclrc
