	.file	"avcall-rs6000.c"
.toc
.csect .text[PR]
gcc2_compiled.:
__gnu_compiled_c:
	.align 2
	.globl __builtin_avcall
	.globl .__builtin_avcall
.csect __builtin_avcall[DS]
__builtin_avcall:
	.long .__builtin_avcall, TOC[tc0], 0
.csect .text[PR]
.__builtin_avcall:
	.extern __mulh
	.extern __mull
	.extern __divss
	.extern __divus
	.extern __quoss
	.extern __quous
	mflr 0
	stw 28,-16(1)
	stw 29,-12(1)
	stw 30,-8(1)
	stw 31,-4(1)
	stw 0,8(1)
	stwu 1,-1096(1)
	mr 31,3
	lwz 9,20(31)
	li 3,8
	addi 9,9,-32
	subfc 9,31,9
	srawi 9,9,2
	cmpw 0,3,9
	addi 0,1,56
	bc 4,0,L..3
	mr 10,0
	addi 8,31,32
	li 11,32
L..5:
	addi 3,3,1
	lwzx 0,11,8
	cmpw 0,3,9
	stw 0,0(10)
	addi 10,10,4
	addi 11,11,4
	bc 12,0,L..5
L..3:
	lwz 9,1056(31)
	addi 9,9,-1060
	subfc 9,31,9
	srawi. 9,9,3
	bc 12,2,L..8
	cmpwi 0,9,1
	bc 12,2,L..11
	cmpwi 0,9,2
	bc 12,2,L..14
	cmpwi 0,9,3
	bc 12,2,L..17
	cmpwi 0,9,4
	bc 12,2,L..20
	cmpwi 0,9,5
	bc 12,2,L..23
	cmpwi 0,9,6
	bc 12,2,L..26
	cmpwi 0,9,7
	bc 12,2,L..29
	cmpwi 0,9,8
	bc 12,2,L..32
	cmpwi 0,9,9
	bc 12,2,L..35
	cmpwi 0,9,10
	bc 12,2,L..38
	cmpwi 0,9,11
	bc 12,2,L..41
	cmpwi 0,9,12
	bc 12,2,L..44
	lfd 13,1156(31)
L..44:
	lfd 12,1148(31)
L..41:
	lfd 11,1140(31)
L..38:
	lfd 10,1132(31)
L..35:
	lfd 9,1124(31)
L..32:
	lfd 8,1116(31)
L..29:
	lfd 7,1108(31)
L..26:
	lfd 6,1100(31)
L..23:
	lfd 5,1092(31)
L..20:
	lfd 4,1084(31)
L..17:
	lfd 3,1076(31)
L..14:
	lfd 2,1068(31)
L..11:
	lfd 1,1060(31)
L..8:
	lwz 0,0(31)
	lwz 3,32(31)
	lwz 4,36(31)
	lwz 5,40(31)
	lwz 6,44(31)
	lwz 7,48(31)
	lwz 8,52(31)
	lwz 9,56(31)
	lwz 10,60(31)
	mr 28,0
	stw 2,20(1)
	lwz 29,0(28)
	lwz 2,4(28)
	mtlr 29
	lwz 11,8(28)
	blrl
	lwz 2,20(1)
	lwz 9,12(31)
	cmpwi 0,9,1
	mr 11,9
	bc 12,2,L..49
	cmpwi 0,9,0
	bc 12,2,L..101
	cmpwi 0,9,2
	bc 12,2,L..102
	cmpwi 0,9,3
	bc 12,2,L..102
	cmpwi 0,9,4
	bc 12,2,L..102
	cmpwi 0,9,5
	bc 12,2,L..103
	cmpwi 0,9,6
	bc 12,2,L..103
	cmpwi 0,9,7
	bc 12,2,L..101
	cmpwi 0,9,8
	bc 12,2,L..101
	cmpwi 0,9,9
	bc 12,2,L..101
	cmpwi 0,9,10
	bc 12,2,L..101
	addi 0,11,-11
	cmplwi 0,0,1
	bc 4,1,L..104
	cmpwi 0,9,13
	bc 4,2,L..72
	lwz 9,8(31)
	frsp 0,1
	stfs 0,0(9)
	b L..49
L..72:
	cmpwi 0,9,14
	bc 4,2,L..74
	lwz 9,8(31)
	stfd 1,0(9)
	b L..49
L..74:
	cmpwi 0,9,15
	bc 12,2,L..101
	cmpwi 0,9,16
	bc 4,2,L..49
	lwz 0,4(31)
	andi. 28,0,1
	bc 12,2,L..79
	lwz 9,16(31)
	cmpwi 0,9,1
	bc 4,2,L..80
	lwz 9,8(31)
	lbz 0,0(3)
	stb 0,0(9)
	b L..49
L..80:
	cmpwi 0,9,2
	bc 4,2,L..82
	lwz 9,8(31)
	lhz 0,0(3)
	sth 0,0(9)
	b L..49
L..82:
	cmpwi 0,9,4
	bc 4,2,L..84
	lwz 9,8(31)
	lwz 0,0(3)
	stw 0,0(9)
	b L..49
L..84:
	cmpwi 0,9,8
	bc 4,2,L..86
	lwz 11,8(31)
	lwz 0,0(3)
	stw 0,0(11)
	lwz 10,8(31)
	lwz 9,4(3)
	stw 9,4(10)
	b L..49
L..86:
	addi 0,9,3
	srwi 10,0,2
	addic. 10,10,-1
	bc 12,0,L..49
	slwi 11,10,2
L..90:
	lwzx 0,11,3
	lwz 9,8(31)
	addic. 10,10,-1
	stwx 0,11,9
	addi 11,11,-4
	bc 4,0,L..90
	b L..49
L..79:
	andi. 29,0,512
	bc 12,2,L..49
	lwz 0,16(31)
	cmpwi 0,0,1
	bc 4,2,L..94
L..102:
	lwz 9,8(31)
	stb 3,0(9)
	b L..49
L..94:
	cmpwi 0,0,2
	bc 4,2,L..96
L..103:
	lwz 9,8(31)
	sth 3,0(9)
	b L..49
L..96:
	cmpwi 0,0,4
	bc 4,2,L..98
L..101:
	lwz 9,8(31)
	stw 3,0(9)
	b L..49
L..98:
	cmpwi 0,0,8
	bc 4,2,L..49
L..104:
	lwz 9,8(31)
	stw 3,0(9)
	lwz 11,8(31)
	stw 4,4(11)
L..49:
	li 3,0
	la 1,1096(1)
	lwz 0,8(1)
	mtlr 0
	lwz 28,-16(1)
	lwz 29,-12(1)
	lwz 30,-8(1)
	lwz 31,-4(1)
	blr
LT..__builtin_avcall:
	.long 0
	.byte 0,0,32,65,128,4,1,0
	.long 0
	.long LT..__builtin_avcall-.__builtin_avcall
	.short 16
	.byte "__builtin_avcall"
_section_.text:
.csect .data[RW]
	.long _section_.text
