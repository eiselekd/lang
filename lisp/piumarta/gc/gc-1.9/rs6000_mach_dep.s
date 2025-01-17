 # Set up _gc_arrays with labels in the middle
    .csect  data[RW]
    .globl  _gc_arrays
    .globl  aobjfreelist
    .globl  objfreelist
    .align  2
_gc_arrays:
aobjfreelist:
    .space  4*513
objfreelist:
    .space  4*513
 # either hblkmap or hblklist.  Reserve space for HBLK_MAP, which is bigger.
    .space  4*8192

    .csect
    .set   r0,0
    .set   r1,1
    .set   r2,2
    .set   r3,3
    .set   r4,4
    .set   r5,5
    .set   r6,6
    .set   r7,7
    .set   r8,8
    .set   r9,9
    .set   r10,10
    .set   r11,11
    .set   r12,12
    .set   r13,13
    .set   r14,14
    .set   r15,15
    .set   r16,16
    .set   r17,17
    .set   r18,18
    .set   r19,19
    .set   r20,20
    .set   r21,21
    .set   r22,22
    .set   r23,23
    .set   r24,24
    .set   r25,25
    .set   r26,26
    .set   r27,27
    .set   r28,28
    .set   r29,29
    .set   r30,30
    .set   r31,31

 # Mark from machine registers that are saved by C compiler
    .globl  .mark_regs
.mark_regs:
    .extern .tl_mark
    stu	    r1,-64(r1)  # reserve stack frame
    mflr    r0		# save link register
    st      r0,0x48(r1)
    oril    r3,r2,0x0   # mark from r2
    bl 	    .tl_mark
    cror    15,15,15
    oril    r3,r13,0x0   # mark from r13-r31
    bl 	    .tl_mark
    cror    15,15,15
    oril    r3,r14,0x0
    bl 	    .tl_mark
    cror    15,15,15
    oril    r3,r15,0x0
    bl 	    .tl_mark
    cror    15,15,15
    oril    r3,r16,0x0
    bl 	    .tl_mark
    cror    15,15,15
    oril    r3,r17,0x0
    bl 	    .tl_mark
    cror    15,15,15
    oril    r3,r18,0x0
    bl 	    .tl_mark
    cror    15,15,15
    oril    r3,r19,0x0
    bl 	    .tl_mark
    cror    15,15,15
    oril    r3,r20,0x0
    bl 	    .tl_mark
    cror    15,15,15
    oril    r3,r21,0x0
    bl 	    .tl_mark
    cror    15,15,15
    oril    r3,r22,0x0
    bl 	    .tl_mark
    cror    15,15,15
    oril    r3,r23,0x0
    bl 	    .tl_mark
    cror    15,15,15
    oril    r3,r24,0x0
    bl 	    .tl_mark
    cror    15,15,15
    oril    r3,r25,0x0
    bl 	    .tl_mark
    cror    15,15,15
    oril    r3,r26,0x0
    bl 	    .tl_mark
    cror    15,15,15
    oril    r3,r27,0x0
    bl 	    .tl_mark
    cror    15,15,15
    oril    r3,r28,0x0
    bl 	    .tl_mark
    cror    15,15,15
    oril    r3,r29,0x0
    bl 	    .tl_mark
    cror    15,15,15
    oril    r3,r30,0x0
    bl 	    .tl_mark
    cror    15,15,15
    oril    r3,r31,0x0
    bl 	    .tl_mark
    cror    15,15,15
    l       r0,0x48(r1)
    mtlr    r0
    ai      r1,r1,64
    br
