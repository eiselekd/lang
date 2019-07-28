#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>


  static void
  call_ocaml_void (const char * name)
  {   CAMLparam0 () ;
      CAMLlocal1 (ostr) ;

      ostr = caml_copy_string (name);

      value * func = caml_named_value ("ocaml_puts") ;

      if (func == NULL)
          puts ("caml_named_value failed!") ;
      else
          caml_callback (*func, ostr) ;

      CAMLreturn0 ;
  } /* call_ocaml_void */


  static void
  call_ocaml_string (char * join, char const ** argv)
  {   CAMLparam0 () ;
      CAMLlocal3 (ojoin, oargv, ores) ;

      ojoin = caml_copy_string (join);
      oargv = caml_alloc_array (caml_copy_string, argv) ;

      value * func = caml_named_value ("ocaml_string_join") ;

      if (func == NULL)
          puts ("caml_named_value failed!") ;
      else
          ores = caml_callback2 (*func, ojoin, oargv) ;

      printf ("Ocaml returned : '%s'\n", String_val (ores)) ;

      CAMLreturn0 ;
  } /* call_ocaml_string */


  int
  main (int argc, char ** argv)
  {   const char * progname ;
      int k, count ;

      progname = argv [0] ;
      if (strstr (progname, "./") == progname)
          progname += 2 ;

      if (argc < 2)
      {   puts ("Need at least 1 command line argument.") ;
          exit (1) ;
          } ;

      count = argc >= 2 ? atoi (argv [1]) : 1 ;
      count = count < 1 ? 1 : count ;
      printf ("Count : %d\n", count) ;

      /* Must call this before calling any Ocaml code. */
      caml_startup (argv) ;

      for (k = 0 ; k < count ; k++)
          call_ocaml_void (progname) ;

      for (k = 0 ; k < count ; k++)
          call_ocaml_string (" ", (char const **) (argv + 1)) ;

      return 0 ;
  } /* main */
