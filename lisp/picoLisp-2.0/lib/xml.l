# 14jun02abu
# (c) Software Lab. Alexander Burger

# Check header
(de xml? ()
   (skip)
   (= '("<" "?" "x" "m" "l") (head 5 (line))) )

# Parse XML data
(de xml (Lst N)
   (if Lst
      (let Tag (pop 'Lst)
         (space (default N 0))
         (prin "<" Tag)
         (mapc
            '((X)
               (prin " " (car X) "=\"")
               (escXml (cdr X))
               (prin "\"") )
            (pop 'Lst) )
         (ifn Lst
            (prinl "/>")
            (prinl ">")
            (mapc
               '((X)
                  (if (pair X)
                     (xml X (+ 3 N))
                     (space (+ 3 N))
                     (escXml X)
                     (prinl) ) )
               Lst )
            (space N)
            (prinl "</" Tag ">") ) )
      (skip)
      (unless (= "<" (char))
         (quit "Bad XML") )
      (_xml (till " /<>" T)) ) )

(de _xml (Tok)
   (use X
      (make
         (link (intern Tok))
         (let L
            (make
               (do NIL
                  (NIL (skip))
                  (T (member @ '`(chop "/>")))
                  (NIL (setq X (intern (till "=" T))))
                  (char)
                  (unless (= "\"" (char))
                     (quit "XML parse error" X) )
                  (link (cons X (pack (xmlEsc (till "\"")))))
                  (char) ) )
            (if (= "/" (char))
               (prog (char) (and L (link L)))
               (link L)
               (do NIL
                  (NIL (skip))
                  (T (and (= "<" (setq X (char))) (= "/" (peek)))
                     (char)
                     (unless (= Tok (till " /<>" T))
                        (quit "Unbalanced XML" Tok) )
                     (char) )
                  (if (= "<" X)
                     (and (_xml (till " /<>" T)) (link @))
                     (link
                        (pack (xmlEsc (trim (cons X (till "^J/<>"))))) ) ) ) ) ) ) ) )

(de xmlEsc (L)
   (use (@A @X @Z)
      (make
         (while L
            (ifn (match '("&" @X ";" @Z) L)
               (link (pop 'L))
               (link
                  (cond
                     ((= @X '`(chop "quot")) "\"")
                     ((= @X '`(chop "amp")) "&")
                     ((= @X '`(chop "lt")) "<")
                     ((= @X '`(chop "gt")) ">")
                     ((= @X '`(chop "apos")) "'")
                     ((= "#" (car @X))
                        (char
                           (if (= "x" (cadr @X))
                              (hex (cddr @X))
                              (format (pack (cdr @X))) ) ) )
                     (T @X) ) )
               (setq L @Z) ) ) ) ) )

(de escXml (X)
   (mapc
      '((C)
         (if (member C '`(chop "\"&<"))
            (prin "&#" (char C) ";")
            (prin C) ) )
      (chop X) ) )

# Access functions
(de body (Lst . @)
   (while (and (setq Lst (cddr Lst)) (next))
      (setq Lst (assoc (arg) Lst)) )
   Lst )

(de attr (Lst . @)
   (let Key (next)
      (while (next)
         (setq Lst (assoc Key (cddr Lst)))
         (setq Key (arg)) )
      (cdr (assoc Key (cadr Lst))) ) )
