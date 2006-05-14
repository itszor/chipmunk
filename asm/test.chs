// %start _y_start

global _y_start

	data
_init:
	dcw 100 + _external
	dcw 200
_bleh:
	dcw 150
	dcw 352
	enddata

weak _bleh

	rodata
_foo:
	dcs "Hello world"
	dcs "this "
_isnot:
	dcs "is not "
	dcb 10
	align 4
_jumptable:
	dcw _jump_0
	dcw _jump_1
	dcw _jump_2
	dcw _jump_3
	dcw _jump_4
	enddata

_jump_0:
	jump _default

_jump_1:
	jump _default

_jump_2:
	jump _default

_jump_3:
	jump _default

_jump_4:
	jump _default

_default:
	jump _y_start

_switch:
	mvc.l r0,#_jumptable
	mvc.h r0,#_jumptable
	lsl r1,r1,#2
	ldr.w r1,@0[~r0,~r1]
	jump r1

_y_start:
	mov r7,#65536
	ior r7,~r7,#36864
	mov r5,#0
	mvc.l r3,#_external
	mvc.h r3,#_external
	utrap.lt _fishcake, ~r5, #57
	jump _x_start

_x_start:
	mov r6,#0
	jump _x_body

_x_body:
	mvc.l r15,#_bleh
	mvc.h r15,#_bleh
	mov r1,r7
	mov r2,r6
	mov r3,r5
	mov r4,#15
	call _putpixel, _x_end

_x_end:
	add r6,r6,#1
	trap.le _x_body,~r6,#100

	add r5,r5,#1
	cmp.gt r9,~r5,#100
	fork r9, _done, _x_start

_done:
	jump _done

_putpixel:
	stm.db r13!,@0{r10-r20}
	mvc.el r11, #320
	mul r10, ~r3, r11
	add r10, ~r10, ~r2
	str.b ~r4, @0[~r1, ~r10]
	ldm.ia r13!, @0{r10-r20}
	ret

