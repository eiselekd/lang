	.SPACE $PRIVATE$
	.SUBSPA $DATA$,QUAD=1,ALIGN=8,ACCESS=31
	.SUBSPA $BSS$,QUAD=1,ALIGN=8,ACCESS=31,ZERO,SORT=82
	.SPACE $TEXT$
	.SUBSPA $LIT$,QUAD=0,ALIGN=8,ACCESS=44
	.SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY
	.IMPORT $global$,DATA
	.IMPORT $$dyncall,MILLICODE
; gcc_compiled.:
	.IMPORT vacall_function,DATA
	.SPACE $TEXT$
	.SUBSPA $CODE$

	.align 4
	.EXPORT vacall,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR,ARGW2=GR,ARGW3=GR
vacall
	.PROC
	.CALLINFO FRAME=192,CALLS,SAVE_RP
	.ENTRY
	stw %r2,-20(0,%r30)
	ldo 192(%r30),%r30
	stw %r26,-228(0,%r30)
	stw %r25,-232(0,%r30)
	stw %r24,-236(0,%r30)
	ldo -120(%r30),%r19
	fstds %fr5,8(0,%r19)
	fstds %fr7,0(0,%r19)
	fstws %fr4L,-4(0,%r19)
	fstws %fr5L,-8(0,%r19)
	fstws %fr6L,-12(0,%r19)
	fstws %fr7L,-16(0,%r19)
	stw %r23,-240(0,%r30)
	stw 0,-184(0,%r30)
	ldo -224(%r30),%r19
	stw %r19,-180(0,%r30)
	stw 0,-176(0,%r30)
	stw 0,-172(0,%r30)
	stw %r28,-152(0,%r30)
	ldo -240(%r30),%r19
	stw %r19,-148(0,%r30)
	ldi 104,%r19
	stw %r19,-144(0,%r30)
	ldi 120,%r19
	stw %r19,-140(0,%r30)
	addil L'vacall_function-$global$,%r27
	ldw R'vacall_function-$global$(%r1),%r19
	ldo -184(%r30),%r26
	copy %r19,%r22
	.CALL	ARGW0=GR
	bl $$dyncall,%r31
	copy %r31,%r2
	ldw -172(0,%r30),%r19
	comib,= 0,%r19,L$0065
	ldw -212(0,%r30),%r2
	comiclr,= 1,%r19,0
	comib,<>,n 2,%r19,L$0006
L$0057
	ldb -160(0,%r30),%r19
	bl L$0065,0
	extrs %r19,31,8,%r28
L$0006
	comib,<>,n 3,%r19,L$0008
	ldb -160(0,%r30),%r28
	bl L$0065,0
	ldw -212(0,%r30),%r2
L$0008
	comib,<>,n 4,%r19,L$0010
	ldh -160(0,%r30),%r19
	bl L$0003,0
	extrs %r19,31,16,%r28
L$0010
	comib,<>,n 5,%r19,L$0012
	ldh -160(0,%r30),%r28
	bl L$0065,0
	ldw -212(0,%r30),%r2
L$0012
	comib,=,n 6,%r19,L$0060
	comib,=,n 7,%r19,L$0060
	comib,=,n 8,%r19,L$0060
	comib,= 9,%r19,L$0060
	ldo -10(%r19),%r19
	comib,>>= 1,%r19,L$0059
	ldw -172(0,%r30),%r19
	comib,<>,n 12,%r19,L$0024
	ldo -152(%r30),%r19
	fldws -8(0,%r19),%fr4L
L$0060
	bl L$0003,0
	ldw -160(0,%r30),%r28
L$0024
	comib,<>,n 13,%r19,L$0026
	ldo -152(%r30),%r19
	fldds -8(0,%r19),%fr4
L$0059
	ldw -160(0,%r30),%r28
	bl L$0003,0
	ldw -156(0,%r30),%r29
L$0026
	comiclr,<> 14,%r19,0
	bl,n L$0060,0
	comib,<> 15,%r19,L$0065
	ldw -212(0,%r30),%r2
	ldw -184(0,%r30),%r19
	bb,>=,n %r19,31,L$0031
	ldw -176(0,%r30),%r28
	bl,n L$0003,0
L$0031
	bb,>= %r19,30,L$0065
	ldw -212(0,%r30),%r2
	bb,>= %r19,28,L$0034
	ldw -168(0,%r30),%r19
	comib,=,n 1,%r19,L$0061
	comib,<>,n 2,%r19,L$0037
	ldw -176(0,%r30),%r19
	bl L$0065,0
	ldh 0(0,%r19),%r28
L$0037
	comib,<> 4,%r19,L$0065
	ldw -212(0,%r30),%r2
	ldw -176(0,%r30),%r19
	bl L$0065,0
	ldw 0(0,%r19),%r28
L$0034
	comib,= 0,%r19,L$0065
	ldw -212(0,%r30),%r2
	comib,<<,n 8,%r19,L$0065
	comib,<>,n 1,%r19,L$0042
L$0061
	ldw -176(0,%r30),%r19
	bl L$0003,0
	ldb 0(0,%r19),%r28
L$0042
	comib,<>,n 2,%r19,L$0044
	ldw -176(0,%r30),%r19
	ldb 0(0,%r19),%r20
	ldb 1(0,%r19),%r19
	zdep %r20,23,24,%r20
	bl L$0003,0
	or %r20,%r19,%r28
L$0044
	comib,<> 3,%r19,L$0046
	ldw -176(0,%r30),%r21
	ldb 0(0,%r21),%r19
	ldb 1(0,%r21),%r20
	ldb 2(0,%r21),%r21
	zdep %r19,15,16,%r19
	zdep %r20,23,24,%r20
	or %r19,%r20,%r19
	bl L$0003,0
	or %r19,%r21,%r28
L$0046
	comib,= 4,%r19,L$0062
	ldw -176(0,%r30),%r22
	comib,<>,n 5,%r19,L$0050
	ldb 4(0,%r22),%r29
L$0062
	ldb 0(0,%r22),%r19
	ldb 1(0,%r22),%r20
	ldb 2(0,%r22),%r21
	zdep %r19,7,8,%r19
	zdep %r20,15,16,%r20
	or %r19,%r20,%r19
	zdep %r21,23,24,%r21
	ldb 3(0,%r22),%r20
	or %r19,%r21,%r19
	bl L$0003,0
	or %r19,%r20,%r28
L$0050
	comib,<> 6,%r19,L$0052
	ldw -176(0,%r30),%r22
	ldb 0(0,%r22),%r19
	ldb 1(0,%r22),%r20
	ldb 2(0,%r22),%r21
	zdep %r19,7,8,%r19
	zdep %r20,15,16,%r20
	or %r19,%r20,%r19
	zdep %r21,23,24,%r21
	ldb 3(0,%r22),%r20
	or %r19,%r21,%r19
	or %r19,%r20,%r28
	ldb 4(0,%r22),%r19
	ldb 5(0,%r22),%r20
	bl L$0063,0
	zdep %r19,23,24,%r19
L$0052
	comib,<>,n 7,%r19,L$0054
	ldb 0(0,%r22),%r19
	ldb 1(0,%r22),%r20
	ldb 2(0,%r22),%r21
	zdep %r19,7,8,%r19
	zdep %r20,15,16,%r20
	or %r19,%r20,%r19
	zdep %r21,23,24,%r21
	or %r19,%r21,%r19
	ldb 3(0,%r22),%r20
	ldb 6(0,%r22),%r21
	or %r19,%r20,%r28
	ldb 4(0,%r22),%r19
	ldb 5(0,%r22),%r20
	zdep %r19,15,16,%r19
	zdep %r20,23,24,%r20
	or %r19,%r20,%r19
	bl L$0003,0
	or %r19,%r21,%r29
L$0054
	comib,<> 8,%r19,L$0065
	ldw -212(0,%r30),%r2
	ldw -176(0,%r30),%r22
	ldb 0(0,%r22),%r19
	ldb 1(0,%r22),%r20
	ldb 2(0,%r22),%r21
	zdep %r19,7,8,%r19
	zdep %r20,15,16,%r20
	or %r19,%r20,%r19
	zdep %r21,23,24,%r21
	or %r19,%r21,%r19
	ldb 3(0,%r22),%r20
	ldb 6(0,%r22),%r21
	or %r19,%r20,%r28
	zdep %r21,23,24,%r21
	ldb 4(0,%r22),%r19
	ldb 5(0,%r22),%r20
	zdep %r19,7,8,%r19
	zdep %r20,15,16,%r20
	or %r19,%r20,%r19
	ldb 7(0,%r22),%r20
	or %r19,%r21,%r19
L$0063
	or %r19,%r20,%r29
L$0003
	ldw -212(0,%r30),%r2
L$0065
	bv 0(%r2)
	ldo -192(%r30),%r30
	.EXIT
	.PROCEND
