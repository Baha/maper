SRC     = src
INCLUDE = include
EBIN    = ebin
ERLC    = @erlc

ERLC_INCLUDE = -I $(INCLUDE)

HRL_FILES = \
	maper_macros

SRC_MODULES = \
	erl2fact

ERL_DIRS = \
	$(SRC)

HRL_DIRS = \
	$(INCLUDE)

vpath %.erl $(ERL_DIRS)

vpath %.hrl $(HRL_DIRS)

default: maper_target

maper_target: $(SRC_MODULES:%=$(EBIN)/%.beam)

$(EBIN)/%.beam: %.erl
	$(ERLC) $(ERLC_INCLUDE) -o $(EBIN) $<
