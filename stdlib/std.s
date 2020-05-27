	.arch armv6
	.eabi_attribute 28, 1
	.eabi_attribute 20, 1
	.eabi_attribute 21, 1
	.eabi_attribute 23, 3
	.eabi_attribute 24, 1
	.eabi_attribute 25, 1
	.eabi_attribute 26, 2
	.eabi_attribute 30, 6
	.eabi_attribute 34, 1
	.eabi_attribute 18, 4
	.file	"std.c"
	.text
	.align	2
	.global	__std_scan_char
	.arch armv6
	.syntax unified
	.arm
	.fpu vfp
	.type	__std_scan_char, %function
__std_scan_char:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	bl	getchar(PLT)
	mov	r3, r0
	mov	r0, r3
	pop	{fp, pc}
	.size	__std_scan_char, .-__std_scan_char
	.section	.rodata
	.align	2
.LC0:
	.ascii	"%d\000"
	.text
	.align	2
	.global	__std_scan_int
	.syntax unified
	.arm
	.fpu vfp
	.type	__std_scan_int, %function
__std_scan_int:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	ldr	r2, .L6
.LPIC1:
	add	r2, pc, r2
	ldr	r3, .L6+4
	ldr	r3, [r2, r3]
	ldr	r3, [r3]
	str	r3, [fp, #-8]
	mov	r3,#0
	sub	r3, fp, #12
	mov	r1, r3
	ldr	r3, .L6+8
.LPIC0:
	add	r3, pc, r3
	mov	r0, r3
	bl	__isoc99_scanf(PLT)
	ldr	r3, [fp, #-12]
	ldr	r1, .L6+12
.LPIC2:
	add	r1, pc, r1
	ldr	r2, .L6+4
	ldr	r2, [r1, r2]
	ldr	r1, [r2]
	ldr	r2, [fp, #-8]
	eors	r1, r2, r1
	beq	.L5
	bl	__stack_chk_fail(PLT)
.L5:
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
.L7:
	.align	2
.L6:
	.word	_GLOBAL_OFFSET_TABLE_-(.LPIC1+8)
	.word	__stack_chk_guard(GOT)
	.word	.LC0-(.LPIC0+8)
	.word	_GLOBAL_OFFSET_TABLE_-(.LPIC2+8)
	.size	__std_scan_int, .-__std_scan_int
	.section	.rodata
	.align	2
.LC1:
	.ascii	"%lf\000"
	.text
	.align	2
	.global	__std_scan_double
	.syntax unified
	.arm
	.fpu vfp
	.type	__std_scan_double, %function
__std_scan_double:
	@ args = 0, pretend = 0, frame = 16
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #16
	ldr	r2, .L11
.LPIC4:
	add	r2, pc, r2
	ldr	r3, .L11+4
	ldr	r3, [r2, r3]
	ldr	r3, [r3]
	str	r3, [fp, #-8]
	mov	r3,#0
	sub	r3, fp, #20
	mov	r1, r3
	ldr	r3, .L11+8
.LPIC3:
	add	r3, pc, r3
	mov	r0, r3
	bl	__isoc99_scanf(PLT)
	ldrd	r2, [fp, #-20]
	vmov	d7, r2, r3
	ldr	r2, .L11+12
.LPIC5:
	add	r2, pc, r2
	ldr	r3, .L11+4
	ldr	r3, [r2, r3]
	ldr	r2, [r3]
	ldr	r3, [fp, #-8]
	eors	r2, r3, r2
	beq	.L10
	bl	__stack_chk_fail(PLT)
.L10:
	vmov.f64	d0, d7
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
.L12:
	.align	2
.L11:
	.word	_GLOBAL_OFFSET_TABLE_-(.LPIC4+8)
	.word	__stack_chk_guard(GOT)
	.word	.LC1-(.LPIC3+8)
	.word	_GLOBAL_OFFSET_TABLE_-(.LPIC5+8)
	.size	__std_scan_double, .-__std_scan_double
	.align	2
	.global	__std_put_str
	.syntax unified
	.arm
	.fpu vfp
	.type	__std_put_str, %function
__std_put_str:
	@ args = 0, pretend = 0, frame = 16
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #16
	str	r0, [fp, #-16]
	str	r1, [fp, #-20]
	mov	r3, #0
	str	r3, [fp, #-8]
	b	.L14
.L15:
	ldr	r3, [fp, #-8]
	ldr	r2, [fp, #-16]
	add	r3, r2, r3
	ldrb	r3, [r3]	@ zero_extendqisi2
	mov	r0, r3
	bl	putchar(PLT)
	ldr	r3, [fp, #-8]
	add	r3, r3, #1
	str	r3, [fp, #-8]
.L14:
	ldr	r2, [fp, #-8]
	ldr	r3, [fp, #-20]
	cmp	r2, r3
	blt	.L15
	nop
	nop
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
	.size	__std_put_str, .-__std_put_str
	.align	2
	.global	__std_put_char
	.syntax unified
	.arm
	.fpu vfp
	.type	__std_put_char, %function
__std_put_char:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	str	r0, [fp, #-8]
	ldr	r0, [fp, #-8]
	bl	putchar(PLT)
	nop
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
	.size	__std_put_char, .-__std_put_char
	.align	2
	.global	__std_put_int
	.syntax unified
	.arm
	.fpu vfp
	.type	__std_put_int, %function
__std_put_int:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	str	r0, [fp, #-8]
	ldr	r1, [fp, #-8]
	ldr	r3, .L18
.LPIC6:
	add	r3, pc, r3
	mov	r0, r3
	bl	printf(PLT)
	nop
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
.L19:
	.align	2
.L18:
	.word	.LC0-(.LPIC6+8)
	.size	__std_put_int, .-__std_put_int
	.align	2
	.global	__std_put_double
	.syntax unified
	.arm
	.fpu vfp
	.type	__std_put_double, %function
__std_put_double:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	vstr.64	d0, [fp, #-12]
	ldrd	r2, [fp, #-12]
	ldr	r1, .L21
.LPIC7:
	add	r1, pc, r1
	mov	r0, r1
	bl	printf(PLT)
	nop
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
.L22:
	.align	2
.L21:
	.word	.LC1-(.LPIC7+8)
	.size	__std_put_double, .-__std_put_double
	.ident	"GCC: (GNU) 9.1.0"
	.section	.note.GNU-stack,"",%progbits
