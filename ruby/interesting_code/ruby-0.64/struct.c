/************************************************

  struct.c -

  $Author: matz $
  $Date: 1995/01/10 10:43:02 $
  created at: Tue Mar 22 18:44:30 JST 1994

************************************************/

#include "ruby.h"
#include "env.h"

VALUE C_Struct;
extern VALUE M_Enumerable;

char *strdup();

static VALUE
struct_alloc(class, name)
    VALUE class;
    char *name;
{
    NEWOBJ(st, struct RStruct);
    OBJSETUP(st, class, T_STRUCT);

    if (name) st->name = strdup(name);
    else st->name = Qnil;
    st->len = 0;
    st->tbl = Qnil;

    return (VALUE)st;
}

static VALUE
struct_find(s, id)
    struct RStruct *s;
    ID id;
{
    struct kv_pair *t, *tend;

    t = s->tbl;
    tend = t + s->len;
    while (t < tend) {
	if (t->key == id) return t->value;
	t++;
    }
    Fail("struct %s has no member %s", s->name, rb_id2name(id));
}

static VALUE
Fstruct_access(s)
    struct RStruct *s;
{
    return struct_find(s, the_env->last_func);
}

static VALUE
struct_add(s, mem, val)
    struct RStruct *s;
    char *mem;
    VALUE val;
{
    int pos = s->len;

    s->len++;
    if (s->tbl == Qnil) {
	s->tbl = ALLOC_N(struct kv_pair, 1);
    }
    else {
	REALLOC_N(s->tbl, struct kv_pair, s->len);
    }

    s->tbl[pos].key = rb_intern(mem);
    s->tbl[pos].value = val;
    rb_define_single_method(s, mem, Fstruct_access, 0);
}

#include <stdarg.h>
//#include <varargs.h>

VALUE
struct_new(char *name, ...)
{
    VALUE st;
    va_list args;
    char *mem;

    st = struct_alloc(C_Struct,name);
    va_start(args, name);
    while (mem = va_arg(args, char*)) {
	struct_add(st, mem, va_arg(args, VALUE));
    }
    va_end(args);

    return st;
}

#define ASSOC_KEY(a) RCONS(a)->car
#define ASSOC_VAL(a) RCONS(a)->cdr

static VALUE
Sstruct_new(argc, argv, class)
    int argc;
    VALUE *argv;
    VALUE class;
{
    VALUE name, st;
    struct RArray *tbl;
    int i, max;

    rb_scan_args(argc, argv, "1*", &name, &tbl);
    Check_Type(name, T_STRING);

    st = struct_alloc(class, RSTRING(name)->ptr);
    for (i=0, max=tbl->len; i<max; i++) {
	VALUE assoc = tbl->ptr[i];

	Check_Type(assoc, T_CONS);
	Check_Type(ASSOC_KEY(assoc), T_STRING);
	struct_add(st, RSTRING(ASSOC_KEY(assoc))->ptr, ASSOC_VAL(assoc));
    }

    return st;
}

static VALUE
Fstruct_each(s)
    struct RStruct *s;
{
    struct kv_pair *t, *tend;

    t = s->tbl;
    tend = t + s->len;
    while (t < tend) {
	rb_yield(t->value);
	t++;
    }
}

static VALUE
Fstruct_values(s)
    struct RStruct *s;
{
    VALUE ary;
    struct kv_pair *t, *tend;

    ary = ary_new();
    t = s->tbl;
    tend = t + s->len;
    while (t < tend) {
	ary_push(ary, t->value);
	t++;
    }

    return ary;
}

static VALUE
Fstruct_aref(s, idx)
    struct RStruct *s;
    VALUE idx;
{
    struct RArray *ary;
    int i;

    if (TYPE(idx) == T_STRING)
	return struct_find(rb_intern(RSTRING(idx)->ptr));
	
    i = NUM2INT(idx);
    if (s->len <= i)
	Fail("offset %d too large for struct(size:%d)", i, s->len);
    return s->tbl[i].value;
}

#define HDR "struct "

static VALUE
Fstruct_to_s(s)
    struct RStruct *s;
{
    char *buf;

    buf = ALLOCA_N(char, strlen(s->name)+sizeof(HDR)+1);
    sprintf(buf, "%s%s", HDR, s->name);
    return str_new2(buf);
}

static VALUE
Fstruct_inspect(s)
    struct RStruct *s;
{
    VALUE str, str2;
    char buf[256], *p;
    int i;
    ID inspect = rb_intern("_inspect");

    sprintf(buf, "#<%s%s: ", HDR, s->name);
    str = str_new2(buf);
    for (i=0; i<s->len; i++) {
	if (i > 0) {
	    str_cat(str, ", ", 2);
	}
	p = rb_id2name(s->tbl[i].key);
	str_cat(str, p, strlen(p));
	str_cat(str, "=", 1);
	str2 = rb_funcall(s->tbl[i].value, inspect, 0, Qnil);
	str_cat(str, RSTRING(str2)->ptr, RSTRING(str2)->len);
    }
    str_cat(str, ">", 1);

    return str;
}

static VALUE
Fstruct_to_a(s)
    struct RStruct *s;
{
    VALUE ary;
    int i;

    ary = ary_new2(s->len);
    for (i=0; i<s->len; i++) {
	ary_push(ary, s->tbl[i].value);
    }

    return ary;
}

static VALUE
Fstruct_clone(s)
    struct RStruct *s;
{
    struct RStruct *st = (struct RStruct*)struct_alloc(s->name);

    CLONESETUP(st, s);
    st->len = s->len;
    st->tbl = ALLOC_N(struct kv_pair, s->len);
    MEMCPY(st->tbl, s->tbl, struct kv_pair, st->len);
    RBASIC(st)->class = single_class_clone(RBASIC(s)->class);
    return (VALUE)st;
}

Init_Struct()
{
    C_Struct = rb_define_class("Struct", C_Object);
    rb_include_module(C_Struct, M_Enumerable);

    rb_define_single_method(C_Struct, "new", Sstruct_new, -1);
    rb_define_method(C_Struct, "clone", Fstruct_clone, 0);

    rb_define_method(C_Struct, "to_s", Fstruct_to_s, 0);
    rb_define_method(C_Struct, "_inspect", Fstruct_inspect, 0);
    rb_define_method(C_Struct, "to_a", Fstruct_to_a, 0);

    rb_define_method(C_Struct, "each", Fstruct_each, 0);
    rb_define_method(C_Struct, "values", Fstruct_values, 0);
    rb_define_method(C_Struct, "[]", Fstruct_aref, 1);
}
