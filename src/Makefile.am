## Process this file with automake to generate a Makefile.in
## To build all programs with GTK+ uncomment these lines.
##AM_CPPFLAGS = @GTK_CFLAGS@
##AM_LDADD = @GTK_LIBS@

bin_PROGRAMS = sim
sim_SOURCES = main.c callbacks.c callbacks.h game.c game.h

## Only our GUI program uses GTK+ at the moment.
sim_CPPFLAGS = @GTK_CFLAGS@
sim_LDADD = @GTK_LIBS@

CLEANFILES = *~ *.orig
