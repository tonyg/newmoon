SUBDIRS = compiler/backend/dotnet lib
DEV_TREE = $(CURDIR)
export DEV_TREE

include Makefile.global
