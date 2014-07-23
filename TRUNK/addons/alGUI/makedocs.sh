#!/bin/sh
## Builds documentation.

pasdoc -T alGUI -O html --cache-dir cache -E docs --auto-abstract lib/*.pas
