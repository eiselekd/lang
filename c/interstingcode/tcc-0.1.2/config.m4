dnl config.m4 for extension tcc

dnl Comments in this file start with the string 'dnl'.
dnl Remove where necessary. This file will not work
dnl without editing.

dnl If your extension references something external, use with:

PHP_ARG_WITH(tcc, for tcc support,
Make sure that the comment is aligned:
[  --with-tcc             Include tcc support])

if test "$PHP_TCC" != "no"; then
  dnl Write more examples of tests here...

  # --with-tcc -> check with-path
  SEARCH_PATH="/usr/local /usr"     # you might want to change this
  SEARCH_FOR="/include/libtcc.h"  # you most likely want to change this
  if test -r $PHP_TCC/$SEARCH_FOR; then # path given as parameter
    TCC_DIR=$PHP_TCC
  else # search default path list
    AC_MSG_CHECKING([for tcc files in default path])
    for i in $SEARCH_PATH ; do
      if test -r $i/$SEARCH_FOR; then
        TCC_DIR=$i
        AC_MSG_RESULT(found in $i)
      fi
    done
  fi

  if test -z "$TCC_DIR"; then
    AC_MSG_RESULT([not found])
    AC_MSG_ERROR([Please reinstall the tcc distribution])
  fi

  # --with-tcc -> add include path
  PHP_ADD_INCLUDE($TCC_DIR/include)

  # --with-tcc -> check for lib and symbol presence
  LIBNAME=tcc # you may want to change this
  LIBSYMBOL=tcc_new # you most likely want to change this 

  PHP_CHECK_LIBRARY($LIBNAME,$LIBSYMBOL,
  [
    PHP_ADD_LIBRARY_WITH_PATH($LIBNAME, $TCC_DIR/lib, TCC_SHARED_LIBADD)
    AC_DEFINE(HAVE_TCCLIB,1,[ ])
  ],[
    AC_MSG_ERROR([wrong tcc lib version or lib not found])
  ],[
    -L$TCC_DIR/lib -lm -ldl
  ])

  PHP_SUBST(TCC_SHARED_LIBADD)

  PHP_NEW_EXTENSION(tcc, tcc.c, $ext_shared)
fi
