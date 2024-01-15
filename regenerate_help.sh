#!/bin/sh

GENDIR="gen"
mkdir -p "${GENDIR}"
TMPFILE="$(mktemp)"
rst2man MANUAL.rst > "${TMPFILE}"
man "${TMPFILE}" | sed -e '1,5d' > ${GENDIR}/MANUAL.txt