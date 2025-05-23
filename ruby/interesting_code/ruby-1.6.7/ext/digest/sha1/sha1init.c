/* $RoughId: sha1init.c,v 1.2 2001/07/13 19:49:10 knu Exp $ */
/* $Id: sha1init.c,v 1.1.2.1 2001/08/16 07:35:43 knu Exp $ */

#include "digest.h"
#include "sha1.h"

static algo_t sha1 = {
    SHA1_DIGEST_LENGTH,
    sizeof(SHA1_CTX),
    (hash_init_func_t)SHA1_Init,
    (hash_update_func_t)SHA1_Update,
    (hash_end_func_t)SHA1_End,
    (hash_final_func_t)SHA1_Final,
    (hash_equal_func_t)SHA1_Equal,
};

void
Init_sha1()
{
    VALUE mDigest, cDigest_Base, cDigest_SHA1;
    ID id_metadata;

    rb_require("digest.so");

    mDigest = rb_path2class("Digest");
    cDigest_Base = rb_path2class("Digest::Base");

    cDigest_SHA1 = rb_define_class_under(mDigest, "SHA1", cDigest_Base);

    id_metadata = rb_intern("metadata");

    rb_cvar_declare(cDigest_SHA1, id_metadata,
		    Data_Wrap_Struct(rb_cObject, 0, 0, &sha1));
}
