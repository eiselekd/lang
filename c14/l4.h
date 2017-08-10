#ifndef DEFINE_L4_H_
#define DEFINE_L4_H_

#define ALLOC_N(type,n) (type*)malloc(sizeof(type)*(n))
#define ALLOC(type) (type*)malloc(sizeof(type))
#define REALLOC_N(var,type,n) (var)=(type*)realloc((char*)(var),sizeof(type)*(n))

struct _node {
  int typ;
  struct _node *l, *r;
};

struct _node* yycompile(char *f);

#endif
