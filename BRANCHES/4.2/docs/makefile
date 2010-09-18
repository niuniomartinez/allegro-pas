# Guión para la generación de la documentación de Allegro.pas.

#
# Nombre del paquete
#
PACKAGE = "Allegro dot pas"

#
# Rutas
#
ROOT	= ../lib/
DOCDIR	= ./

#
# Archivos de origen
#
SRCMAC = --input=$(ROOT)
SRC = $(SRCMAC)al256tra.pas\
      $(SRCMAC)albase.pas \
      $(SRCMAC)albitmap.pas \
      $(SRCMAC)albltspr.pas \
      $(SRCMAC)alcolor.pas \
      $(SRCMAC)alconfig.pas \
      $(SRCMAC)aldigi.pas \
      $(SRCMAC)aldraw.pas \
      $(SRCMAC)aldtfile.pas \
      $(SRCMAC)alfile.pas \
      $(SRCMAC)alfixed.pas \
      $(SRCMAC)alflic.pas \
      $(SRCMAC)alfont.pas \
      $(SRCMAC)algraph.pas \
      $(SRCMAC)aljstick.pas \
      $(SRCMAC)alkeybrd.pas \
      $(SRCMAC)almidi.pas \
      $(SRCMAC)almouse.pas \
      $(SRCMAC)alpalete.pas \
      $(SRCMAC)alsound.pas \
      $(SRCMAC)alsprrle.pas \
      $(SRCMAC)alsystem.pas \
      $(SRCMAC)altext.pas \
      $(SRCMAC)altimer.pas\
      $(SRCMAC)altrutra.pas
DESCR = $(DOCDIR)docs.xml

#
# Reglas de construcción
#

.PHONY: all

all: docs

docs:
	fpdoc --package=$(PACKAGE) --descr=$(DESCR) $(SRC)

# IF YOU RUN "make skeleton" ALL DOCUMENTATION WILL BE ERASED
# MAKE SURE YOU HAVE A BACKUP COPY OF "docs.xml" BEFORE
skeleton:
	makeskel --package=$(PACKAGE) $(SRC) --output=docs.xml

