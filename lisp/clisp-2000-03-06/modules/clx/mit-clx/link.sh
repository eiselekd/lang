make clisp-module CC="${CC}" CFLAGS="${CFLAGS}" INCLUDES="$absolute_linkkitdir"
NEW_FILES=''
NEW_LIBS=''
NEW_MODULES=''
TO_LOAD="package depdefs clx dependent macros bufmac buffer display gcontext input requests fonts graphics text attributes translate keysyms manager image resource describe trace"
for f in $TO_LOAD; do
  NEW_FILES="$NEW_FILES $f.lsp"
done
for f in $TO_LOAD; do
  NEW_FILES="$NEW_FILES $f.fas"
done
