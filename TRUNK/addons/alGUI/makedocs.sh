#!/bin/sh
## Builds documentation.

pasdoc -T alGUI -O html --cache-dir obj -E docs --auto-abstract lib/*.pas
