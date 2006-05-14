
_block:
	mov r0,r1
	mov r0,#10
	not r2,r3
	not r2,#20
	lsl r1,expire r2,r3
	lsr r1,r2,expire r3
	asr r1,r2,#5
	ror r2,r3,#5
	rrx r1,expire r2,expire r3
	and r1,r2,r3
	or r1,r2,r3
	eor r1,r2,r3
	bic r1,r2,r3
	add r1,r2,r3
	adc r1,r2,r3.c
	sub r1,r2,r3
	sbc r1,r2,r3.c
	rsb r1,r2,r3
	cmp.eq r1,r2,r3
	cmp.ne r1,r2,r3
	cmp.gt r1,r2,r3
	cmp.lt r1,r2,r3
	cmp.ge r1,r2,r3
	cmp.le r1,r2,r3
	cmp.tst r1,r2,r3
	mul r1,r2,r3
	div r1,r2,r3
	udiv r1,r2,r3
	mod r1,r2,r3
	umod r1,r2,r3
	bfi r1 <4,10>, r2
	bfx r1,r2 <5,11>
	ldr.b r1,@0[r2,r3]
	str.h r1,@0[r2,r3]
	str.b expire r1,@3[r2,#10]
	str.t r1,@2[r2,r3]
	str.w expire r1,@2[r2,#32]
	ldm.ia r13,@3{r0-r5}
	stm.db r13,@2{r1-r6}
	jump _block
