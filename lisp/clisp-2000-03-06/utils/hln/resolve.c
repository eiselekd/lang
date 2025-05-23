/* resolve.c -- resolve symbolic links in a pathname

   Copyright (C) 1995-1999 Bruno Haible

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/*
TITLE
     RESOLVE(3)
SYNOPSIS
     char* resolve (const char* path, char resolved_path[PATH_MAX]);
DESCRIPTION
     resolve() expands all symbolic  links  and  resolves  refer-
     ences  to '/./', '/../' and extra '/' characters in the null
     terminated string named by path and stores the canonicalized
     pathname in the buffer named by resolved_path. The resulting
     path will have no symbolic links  components,  nor any '/./'
     or '/../' components.
RETURN VALUES
     resolve() returns a pointer to  the  resolved_path  on  suc-
     cess.   On  failure, it returns NULL, sets errno to indicate
     the error, and places in resolved_path the absolute pathname
     of the path component which could not be resolved.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/types.h>
#include <errno.h>
#ifndef errno
extern int errno;
#endif

#include <sys/param.h>

#include "system.h"

#ifndef MAXSYMLINKS
#define MAXSYMLINKS 8
#endif

#ifndef ELOOP_VALUE
#define ELOOP_VALUE ELOOP
#endif

#ifdef S_ISLNK
int readlink();
#endif

char* resolve(path,resolved_path)
    const char* path;
    char* resolved_path;
{
#ifdef S_ISLNK
  char mypath[PATH_MAX];
  int symlinkcount = 0; /* Anzahl bisher aufgetretener symbolischer Links */
#endif
  char* resolved_limit = &resolved_path[PATH_MAX-1];
  /* G�ltige Pointer sind die mit resolved_path <= ptr <= resolved_limit. */
  /* In *resolved_limit darf h�chstens noch ein Nullbyte stehen. */
  /* (Analog mit mypath.) */
  {
    char* resolved_ptr = resolved_path; /* (bleibt stets <= resolved_limit) */
    /* Dann path selber einkopieren: */
    {
      const char* path_ptr = path;
      while ((resolved_ptr < resolved_limit) && *path_ptr)
        { *resolved_ptr++ = *path_ptr++; }
      /* Mit '/' und einem Nullbyte abschlie�en: */
      if (resolved_ptr < resolved_limit)
        { *resolved_ptr++ = '/'; }
      *resolved_ptr = 0;
  } }
  {
    char* from_ptr = &resolved_path[0];
    char* to_ptr = &resolved_path[0];
    while ((to_ptr < resolved_limit) && (*from_ptr))
      /* Bis hierher hat der Pfad in  resolved_path[0]...to_ptr[-1] */
      /* die Gestalt '[/]subdir1/subdir2/.../txt', */
      /* wobei 'txt' evtl. leer, aber kein subdir leer. */
      {
        char next = *from_ptr++; *to_ptr++ = next;
        if ((next == '/') && (to_ptr >= resolved_path+2))
          /* to_ptr[-1]='/'  ->  Directory ...to_ptr[-2] aufl�sen: */
          {
            char* last_subdir_end = &to_ptr[-2];
            switch (*last_subdir_end)
              {
                case '/':
                  /* '//' wird zu '/' vereinfacht: */
                  to_ptr--;
                  break;
                case '.':
                  {
                    char* last_subdir_ptr = &last_subdir_end[-1];
                    if (to_ptr > resolved_path+2)
                      { 
                        if (*last_subdir_ptr == '.')
                          {
                            if ((to_ptr > resolved_path+4) && (*--last_subdir_ptr == '/'))
                              /* letztes subdir war '/../' */
                              /* Daf�r das subdir davor entfernen: */
                              {
                                while ((last_subdir_ptr > resolved_path) && !(last_subdir_ptr[-1] == '/'))
                                  last_subdir_ptr--;
                                to_ptr = last_subdir_ptr;
                          }   }
                        else if (*last_subdir_ptr == '/')
                          {
                            /* letztes subdir war '/./' */
                            /* entfernen: */
                            to_ptr = last_subdir_end;
                  }   }   }
                  break;
                default:
                  /* nach einem normalen subdir */
#ifdef S_ISLNK
                  /* symbolischen Link lesen: */
                  to_ptr[-1]=0; /* '/' durch 0 ersetzen */
                  {
                    int linklen = readlink(resolved_path,mypath,sizeof(mypath)-1);
                    if (linklen >=0)
                      /* war ein symbolisches Link */
                      {
                        if (++symlinkcount > MAXSYMLINKS)
                          { errno = ELOOP_VALUE; return NULL; }
                        /* noch aufzul�senden path-Anteil an den Link-Inhalt anh�ngen: */
                        {
                          char* mypath_ptr = &mypath[linklen]; /* ab hier ist Platz */
                          char* mypath_limit = &mypath[PATH_MAX-1]; /* bis hierher */
                          if (mypath_ptr < mypath_limit)
                            { *mypath_ptr++ = '/'; } /* erst ein '/' anh�ngen */
                          /* dann den Rest: */
                          while ((mypath_ptr <= mypath_limit) && (*mypath_ptr = *from_ptr++))
                            { mypath_ptr++; }
                          *mypath_ptr = 0; /* und mit 0 abschlie�en */
                        }
                        /* Dies ersetzt bzw. erg�nzt den path: */
                        if (mypath[0] == '/')
                          /* ersetzt den path: */
                          {
                            from_ptr = &mypath[0]; to_ptr = resolved_path;
                            while ((*to_ptr++ = *from_ptr++)) ;
                            from_ptr = resolved_path;
                          }
                          else
                          /* erg�nzt den path: */
                          {
                            /* Linknamen streichen. Dazu bis zum letzten '/' suchen: */
                            {
                              char* ptr = &to_ptr[-1];
                              while ((ptr > resolved_path) && !(ptr[-1] == '/'))
                                ptr--;
                              from_ptr = ptr;
                            }
                            {
                              char* mypath_ptr = &mypath[0]; to_ptr = from_ptr;
                              while ((to_ptr <= resolved_limit) && (*to_ptr++ = *mypath_ptr++));
                          } }
                        to_ptr = from_ptr;
                      }
                      else
                      if ((errno == EINVAL)
#if defined(sgi) || defined(__sgi) /* Irix */
                          || (errno == ENXIO)
#endif
                         )
                        /* kein symbolisches Link */
                        { to_ptr[-1] = '/'; } /* wieder den '/' eintragen */
                      else
                        { return NULL; } /* Fehler */
                  }
#endif
                  break;
          }   }
      } /* dann zum n�chsten subdir */
    /* ein '/' am Ende streichen: */
    if ((to_ptr[-1] == '/') && (to_ptr > resolved_path+1))
      { to_ptr--; }
    to_ptr[0] = 0; /* durch 0 abschlie�en */
    return resolved_path; /* fertig */
} }
