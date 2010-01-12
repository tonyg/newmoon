SUBDIRS = compiler/backend/c lib
DEV_TREE = $(CURDIR)
export DEV_TREE

include Makefile.global
