/* 04oct02abu
 * (c) Software Lab. Alexander Burger
 */

#include "pico.h"

/* Mark data */
static void mark(any x) {
   cell *p;

   while (num((p = cellPtr(x))->cdr) & 1) {
      *(long*)&cdr(p) &= ~1;
      if (!isNum(x))
         mark(p->car);
      x = p->cdr;
   }
}

/* Garbage collector */
static void gc(long c) {
   any p, *pp, x;
   heap *h;
   int i;

   val(DB) = Nil;
   h = Heaps;
   do {
      p = h->cells + CELLS-1;
      do
         *(long*)&cdr(p) |= 1;
      while (--p >= h->cells);
   } while (h = h->next);
   /* Mark */
   mark(Nil+1);
   mark(Line),  mark(Zero);
   for (i = 0; i < HASH; ++i)
      mark(Intern[i]),  mark(Transient[i]);
   mark(ApplyArgs),  mark(ApplyBody);
   for (p = Env.stack; p; p = cdr(p))
      mark(car(p));
   for (p = (any)Env.bind;  p;  p = (any)((bindFrame*)p)->link)
      for (i = ((bindFrame*)p)->cnt;  --i >= 0;)
         mark(((bindFrame*)p)->bnd[i].val);
   for (p = (any)CatchPtr; p; p = (any)((catchFrame*)p)->link)
      mark(((catchFrame*)p)->tag);
   for (p = (any)Env.meth;  p;  p = (any)((methFrame*)p)->link)
      mark(((methFrame*)p)->key),  mark(((methFrame*)p)->cls);
   if (Env.make)
      mark(car(Env.make));
   if (Env.parser)
      mark(Env.parser->name);
   for (i = 0; i < HASH; ++i)
      for (p = Extern[i];  isCell(p);  p = (any)(num(p->cdr) & ~1))
         if (num(tail(p->car)) & 1) {
            for (x = (any)(num(tail(p->car)) & ~1); !isSym(x); x = cdr(cellPtr(x)));
            if ((x = (any)(num(x) & ~1)) == At2  ||  x == At3)
               mark(p->car);  // Keep if dirty or deleted
         }
   if (num(tail(val(DB) = DbVal)) & 1)
      val(DbVal) = cdr(numCell(tail(DbVal) = DbTail)) = Nil;
   for (i = 0; i < HASH; ++i)
      for (pp = Extern + i;  isCell(p = *pp);)
         if (num(tail(p->car)) & 1)
            *pp = (cell*)(num(p->cdr) & ~1);
         else
            *(long*)(pp = &cdr(p)) &= ~1;
   /* Sweep */
   Avail = NULL;
   h = Heaps;
   do {
      p = h->cells + CELLS-1;
      do
         if (num(p->cdr) & 1)
            Free(p),  --c;
      while (--p >= h->cells);
   } while (h = h->next);
   while (c >= 0)
      heapAlloc(),  c -= CELLS;
}

// (gc ['cnt]) -> cnt | NIL
any doGc(any x) {
   x = cdr(x);
   gc(isNum(x = EVAL(car(x)))? 1024*1024/sizeof(cell)*unBox(x) : CELLS);
   return x;
}

/* Construct a cell */
any cons(any x, any y) {
   cell *p;

   if (!(p = Avail)) {
      cell c1, c2;

      Push(c1,x);
      Push(c2,y);
      gc(CELLS);
      drop(c1);
      p = Avail;
   }
   Avail = p->cdr;
   p->car = x;
   p->cdr = y;
   return p;
}

/* Construct a symbol */
any consSym(any v, any x) {
   cell *p;

   if (!(p = Avail)) {
      cell c1, c2;

      Push(c1,v);
      Push(c2,x);
      gc(CELLS);
      drop(c1);
      p = Avail;
   }
   Avail = p->cdr;
   cdr(p) = x;
   p = cellSym(p);
   val(p) = v;
   return p;
}

/* Construct a string */
any consStr(any x) {
   cell *p;

   if (!(p = Avail)) {
      cell c1;

      Push(c1,x);
      gc(CELLS);
      drop(c1);
      p = Avail;
   }
   Avail = p->cdr;
   cdr(p) = x;
   p = cellSym(p);
   val(p) = p;
   return p;
}

/* Construct a number cell */
any consNum(word n, any x) {
   cell *p;

   if (!(p = Avail)) {
      cell c1;

      Push(c1,x);
      gc(CELLS);
      drop(c1);
      p = Avail;
   }
   Avail = p->cdr;
   p->car = (any)n;
   p->cdr = x;
   return cellNum(p);
}
