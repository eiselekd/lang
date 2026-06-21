.section .ctors,"aw",@progbits
.align 4
    .long 0

.section .dtors,"aw",@progbits
.align 4
    .long 0

.section .init_array,"aw",@init_array
.align 4
.global __init_array_end
.hidden __init_array_end
__init_array_end:

.section .fini_array,"aw",@fini_array
.align 4
.global __fini_array_end
.hidden __fini_array_end
__fini_array_end:

