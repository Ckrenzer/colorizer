.PHONY: create-build-image build-executable destroy-build-image build

# set to 'musl' for an alpine build--this ties to the Dockerfile build stage.
C_STANDARD_LIBRARY_IMPLEMENTATION ?= glibc

# create an image with all dependencies needed to build the executable.
create-build-image:
	docker build \
		--target $(C_STANDARD_LIBRARY_IMPLEMENTATION) \
		--tag colorizer-builder:$(C_STANDARD_LIBRARY_IMPLEMENTATION) \
		.

# run the build container with this repo root's filesystem mounted, then build
# the executable.
build-executable:
	docker run \
		--rm \
		-v .:/app \
		--workdir /app \
		colorizer-builder:$(C_STANDARD_LIBRARY_IMPLEMENTATION) sbcl --load app.lisp

# remove the image used to build the executable.
destroy-build-image:
	docker image rm colorizer-builder:$(C_STANDARD_LIBRARY_IMPLEMENTATION)

build: create-build-image build-executable destroy-build-image
