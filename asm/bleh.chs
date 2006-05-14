	data
_bleh:
	dcw 300
	enddata

	global _bleh
	global _fishcake
	global _external

_external:
	mov r30,#16
	ret

	rodata
_fishcake:
	dcw 0
	enddata
