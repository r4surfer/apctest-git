# Makefile for include.h under SCCS Control	

# Definitions are here

DATE=
REL=

GET=get $(DATE) $(REL)

#  This is the default rule - it will get the sources from SCCS
#   and the current objects from the archive file.
#.DEFAULT:
#	$(GET) SCCS/s.$@

all: \
  dostrct.h								      \
  ext_tab.h      set_tab.h      cmsdefs.h      com.h          com0.h          \
  const.h        file.h         file0.h        global.h       grammar.h       \
  grammar0.h     keyword.h      keyword0.h     main.h         main0.h         \
  pos.h          symbol.h       tbomb.h        wsb_print.h    wsb_scrn.h      \
  wvsb.h         wvsb0.h        xmain.h        xmain0.h       yacc.h         

clean:
	rm -rf *.c
