	.text

	.globl	__chigusa_main
__chigusa_main:
	.fnstart
	push	{r11, lr}
	mov	r11,	sp
	sub	sp,	sp,	#8
__chigusa_main$bb0:
	mov	r0,	#1
	str	r0,	[sp, #0]
	bl	__std_scan_int
	str	r0,	[sp, #4]
	bl	__std_scan_int
	ldr	r4,	[sp, #4]
	ldr	r4,	[sp, #4]
	cmp	r4,	#0
	ble	__chigusa_main$bb2
__chigusa_main$bb1:
	ldr	r1,	VAL____chigusa_main$6
	sdiv	r1,	r4,	r1
	ldr	r2,	VAL____chigusa_main$7
	mul	r1,	r1,	r2
	cmp	r1,	r4
	beq	__chigusa_main$bb4
__chigusa_main$bb3:
	ldr	r5,	[sp, #0]
	ldr	r5,	[sp, #0]
	mul	r1,	r5,	r0
	mov	r5,	r1
__chigusa_main$bb4:
	mul	r0,	r0,	r0
	ldr	r0,	VAL____chigusa_main$21
	sdiv	r0,	r4,	r0
	cmp	r4,	#0
	bgt	__chigusa_main$bb1
__chigusa_main$bb2:
	mov	r0,	r5
	bl	__std_put_int
	mov	r0,	#10
	bl	__std_put_char
	b	__chigusa_main$end
__chigusa_main$end:
	mov	sp,	r11
	pop	{r11, pc}
	.fnend
VAL____chigusa_main$21:
		.word	2
VAL____chigusa_main$6:
		.word	2
VAL____chigusa_main$7:
		.word	2

	.globl	main
main:
	.fnstart
	push	{r11, lr}
	mov	r11,	sp
	sub	sp,	sp,	#0
main$bb0:
	bl	__chigusa_main
main$end:
	mov	sp,	r11
	pop	{r11, pc}
	.fnend

