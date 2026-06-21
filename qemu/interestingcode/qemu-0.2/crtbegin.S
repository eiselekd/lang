.section .ctors,"aw",@progbits
.align 4
.global __CTOR_LIST__
__CTOR_LIST__:
    .long -1

.section .dtors,"aw",@progbits
.align 4
.global __DTOR_LIST__
__DTOR_LIST__:
    .long -1

.section .init_array,"aw",@init_array
.align 4
.global __init_array_start
.hidden __init_array_start
__init_array_start:

.section .fini_array,"aw",@fini_array
.align 4
.global __fini_array_start
.hidden __fini_array_start
__fini_array_start:

