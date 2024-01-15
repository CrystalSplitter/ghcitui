#!/bin/sh

GENDIR="gen"
mkdir -p "${GENDIR}"
TMPFILE="$(mktemp)"
rst2man MANUAL.rst > "${TMPFILE}"
MANWIDTH=79 man "${TMPFILE}" | sed -e '1,5d' > ${GENDIR}/MANUAL.txt
