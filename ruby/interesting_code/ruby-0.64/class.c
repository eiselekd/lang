/************************************************

  class.c -

  $Author: matz $
  $Date: 1995/01/10 10:42:21 $
  created at: Tue Aug 10 15:05:44 JST 1993

  Copyright (C) 1994 Yukihiro Matsumoto

************************************************/

#include "ruby.h"
#include "env.h"
#include "node.h"
#include "st.h"

struct st_table *new_idhash();

extern VALUE C_Class;
extern VALUE C_Module;

VALUE
class_new(struct RClass *super)
{
    NEWOBJ(cls, struct RClass);
    OBJSETUP(cls, C_Class, T_CLASS);

    cls->super = super;
    cls->m_tbl = new_idhash();
    cls->c_tbl = Qnil;

    return (VALUE)cls;
}

VALUE
single_class_new(struct RClass *super)
{
    struct RClass *cls = (struct RClass*)class_new(super);

    FL_SET(cls, FL_SINGLE);

    return (VALUE)cls;
}

static int
clone_method(mid, body, tbl)
    ID mid;
    NODE *body;
    st_table *tbl;
{
    st_insert(tbl, mid, NEW_METHOD(body->nd_body, body->nd_noex));
    return ST_CONTINUE;
}

VALUE
single_class_clone(class)
    struct RClass *class;
{
    if (!FL_TEST(class, FL_SINGLE))
	return (VALUE)class;
    else {
	/* copy single(unnamed) class */
	NEWOBJ(clone, struct RClass);
	CLONESETUP(clone, class);

	clone->super = class->super;
	clone->m_tbl = new_idhash();
	st_foreach(class->m_tbl, clone_method, clone->m_tbl);
	clone->c_tbl = Qnil;
	FL_SET(clone, FL_SINGLE);
	return (VALUE)clone;
    }
}

VALUE 
rb_define_class_id(id, super)
    ID id;
    struct RBasic *super;
{
    struct RClass *cls = (struct RClass*)class_new(super);

    rb_name_class(cls, id);

    /* make metaclass */
    RBASIC(cls)->class = single_class_new(super?super->class:C_Class);

    return (VALUE)cls;
}

VALUE 
rb_define_class(char *name, VALUE super)
{
    return rb_define_class_id(rb_intern(name), super);
}

VALUE
module_new()
{
    NEWOBJ(mdl, struct RClass);
    OBJSETUP(mdl, C_Module, T_MODULE);

    mdl->super = Qnil;
    mdl->m_tbl = new_idhash();
    mdl->c_tbl = Qnil;

    return (VALUE)mdl;
}

VALUE 
rb_define_module_id(id)
    ID id;
{
    struct RClass *mdl = (struct RClass*)module_new();

    rb_name_class(mdl, id);
    return (VALUE)mdl;
}

VALUE 
rb_define_module(name)
    char *name;
{
    return rb_define_module_id(rb_intern(name));
}

static struct RClass *
include_class_new(module, super)
    struct RClass *module, *super;
{
    struct RClass *p;

    NEWOBJ(cls, struct RClass);
    OBJSETUP(cls, C_Class, T_ICLASS);

    cls->m_tbl = module->m_tbl;
    cls->c_tbl = module->c_tbl;
    cls->super = super;
    if (TYPE(module) == T_ICLASS) {
	RBASIC(cls)->class = RBASIC(module)->class;
    }
    else {
	RBASIC(cls)->class = (VALUE)module;
    }

    return cls;
}

void
rb_include_module(class, module)
    struct RClass *class, *module;
{
    struct RClass *p;
    int added = FALSE;

    Check_Type(module, T_MODULE);

    while (module) {
	/* ignore if module included already in superclasses */
	for (p = class->super; p; p = p->super) {
	    if (BUILTIN_TYPE(p) == T_ICLASS && p->m_tbl == module->m_tbl)
		goto ignore_module;
	}

	class->super = include_class_new(module, class->super);
	added = TRUE;
	class = class->super;
      ignore_module:
	module = module->super;
    }
    if (added) {
	rb_clear_cache2(class);
    }
}

void
rb_add_method(class, mid, node, noex)
    struct RClass *class;
    ID mid;
    NODE *node;
    int noex;
{
    NODE *body;

    if (class == Qnil) class = (struct RClass*)C_Object;
    if (st_lookup(class->m_tbl, mid, &body)) {
	if (verbose) {
	    Warning("redefine %s", rb_id2name(mid));
	}
	rb_clear_cache(body);
    }
    body = NEW_METHOD(node, noex);
    st_insert(class->m_tbl, mid, body);
}

void
rb_define_method(class, name, func, argc)
    struct RClass *class;
    char *name;
    VALUE (*func)();
    int argc;
{
    rb_add_method(class, rb_intern(name), NEW_CFUNC(func, argc), 0);
}

void
rb_undef_method(class, name)
    struct RClass *class;
    char *name;
{
    rb_add_method(class, rb_intern(name), Qnil, 0);
}

void
rb_define_private_method(class, name, func, argc)
    struct RClass *class;
    char *name;
    VALUE (*func)();
    int argc;
{
    rb_add_method(class, rb_intern(name), NEW_CFUNC(func, argc), 1);
}

VALUE
rb_single_class(obj)
    struct RBasic *obj;
{
    switch (TYPE(obj)) {
      case T_OBJECT:
      case T_CLASS:
      case T_MODULE:
      case T_STRUCT:
	break;
      default:
	Fail("can't define single method for built-in class");
	break;
    }

    if (FL_TEST(obj->class, FL_SINGLE)) {
	return (VALUE)obj->class;
    }
    return obj->class = single_class_new(obj->class);
}

void
rb_define_single_method(obj, name, func, argc)
    VALUE obj;
    char *name;
    VALUE (*func)();
    int argc;
{
    rb_define_method(rb_single_class(obj), name, func, argc);
}

void
rb_define_module_function(module, name, func, argc)
    VALUE module;
    char *name;
    VALUE (*func)();
    int argc;
{
    rb_define_private_method(module, name, func, argc);
    rb_define_single_method(module, name, func, argc);
}

void
rb_define_alias(class, name1, name2)
    struct RClass *class;
    char *name1, *name2;
{
    rb_alias(class, rb_intern(name1), rb_intern(name2));
}

void
rb_define_attr(class, name, pub)
    struct RClass *class;
    char *name;
    int pub;
{
    char *buf;
    ID attr, attreq, attriv;

    attr = rb_intern(name);
    buf = ALLOCA_N(char,strlen(name)+2);
    sprintf(buf, "%s=", name);
    attreq = rb_intern(buf);
    sprintf(buf, "@%s", name);
    attriv = rb_intern(buf);
    if (rb_method_boundp(class, attr) == Qnil) {
	rb_add_method(class, attr, NEW_IVAR(attriv), 0);
    }
    if (pub && rb_method_boundp(class, attreq) == Qnil) {
	rb_add_method(class, attreq, NEW_ATTRSET(attriv), 0);
    }
}

void
rb_define_single_attr(obj, name, pub)
    VALUE obj;
    char *name;
    int pub;
{
    rb_define_attr(rb_single_class(obj), name, pub);
}

#include <stdarg.h>
//#include <varargs.h>
#include <ctype.h>

int
rb_scan_args(int argc, VALUE *argv, char *fmt, ...)
{
    int n, i;
    char *p = fmt;
    VALUE *var;
    va_list vargs;

    va_start(vargs, fmt);

    if (*p == '*') {
	var = va_arg(vargs, VALUE*);
	*var = ary_new4(argc, argv);
	return argc;
    }

    if (isdigit(*p)) {
	n = *p - '0';
	if (n > argc)
	    Fail("Wrong number of arguments (%d for %d)", argc, n);
	for (i=0; i<n; i++) {
	    var = va_arg(vargs, VALUE*);
	    *var = argv[i];
	}
	p++;
    }
    else {
	goto error;
    }

    if (isdigit(*p)) {
	n = i + *p - '0';
	for (; i<n; i++) {
	    var = va_arg(vargs, VALUE*);
	    if (argc > i) {
		*var = argv[i];
	    }
	    else {
		*var = Qnil;
	    }
	}
	p++;
    }

    if(*p == '*') {
	var = va_arg(vargs, VALUE*);
	if (argc > i) {
	    *var = ary_new4(argc-i, argv+i);
	}
	else {
	    *var = ary_new();
	}
    }
    else if (*p == '\0') {
	if (argc > i) {
	    Fail("Wrong # of arguments(%d for %d)", argc, i);
	}
    }
    else {
	goto error;
    }

    va_end(vargs);
    return argc;

  error:
    Fail("bad scan arg format: %s", fmt);
}
