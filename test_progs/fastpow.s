	.text
	.syntax unified
	.eabi_attribute	67, "2.09"	@ Tag_conformance
	.eabi_attribute	6, 10	@ Tag_CPU_arch
	.eabi_attribute	7, 65	@ Tag_CPU_arch_profile
	.eabi_attribute	8, 1	@ Tag_ARM_ISA_use
	.eabi_attribute	9, 2	@ Tag_THUMB_ISA_use
	.fpu	neon
	.eabi_attribute	34, 1	@ Tag_CPU_unaligned_access
	.eabi_attribute	15, 1	@ Tag_ABI_PCS_RW_data
	.eabi_attribute	16, 1	@ Tag_ABI_PCS_RO_data
	.eabi_attribute	17, 2	@ Tag_ABI_PCS_GOT_use
	.eabi_attribute	20, 1	@ Tag_ABI_FP_denormal
	.eabi_attribute	21, 0	@ Tag_ABI_FP_exceptions
	.eabi_attribute	23, 3	@ Tag_ABI_FP_number_model
	.eabi_attribute	24, 1	@ Tag_ABI_align_needed
	.eabi_attribute	25, 1	@ Tag_ABI_align_preserved
	.eabi_attribute	38, 1	@ Tag_ABI_FP_16bit_format
	.eabi_attribute	18, 4	@ Tag_ABI_PCS_wchar_t
	.eabi_attribute	26, 2	@ Tag_ABI_enum_size
	.eabi_attribute	14, 0	@ Tag_ABI_PCS_R9_use
	.file	"fastpow.c"
	.globl	scan                    @ -- Begin function scan
	.p2align	2
	.type	scan,%function
	.code	32                      @ @scan
scan:
	.fnstart
@ %bb.0:
	.save	{r11, lr}
	push	{r11, lr}
	.setfp	r11, sp
	mov	r11, sp
	.pad	#8
	sub	sp, sp, #8
	ldr	r1, .LCPI0_0
.LPC0_0:
	add	r1, pc, r1
	str	r0, [sp, #4]
	ldr	r0, [sp, #4]
	str	r0, [sp]                @ 4-byte Spill
	mov	r0, r1
	ldr	r1, [sp]                @ 4-byte Reload
	bl	printf
	ldr	r1, [sp, #4]
	add	r1, r1, #1
	str	r1, [sp, #4]
	mov	sp, r11
	pop	{r11, pc}
	.p2align	2
@ %bb.1:
.LCPI0_0:
	.long	.L.str-(.LPC0_0+8)
.Lfunc_end0:
	.size	scan, .Lfunc_end0-scan
	.cantunwind
	.fnend
                                        @ -- End function
	.globl	print                   @ -- Begin function print
	.p2align	2
	.type	print,%function
	.code	32                      @ @print
print:
	.fnstart
@ %bb.0:
	.pad	#4
	sub	sp, sp, #4
	str	r0, [sp]
	ldr	r0, [sp]
	add	r0, r0, #1
	str	r0, [sp]
	add	sp, sp, #4
	bx	lr
.Lfunc_end1:
	.size	print, .Lfunc_end1-print
	.cantunwind
	.fnend
                                        @ -- End function
	.globl	main                    @ -- Begin function main
	.p2align	2
	.type	main,%function
	.code	32                      @ @main
main:
	.fnstart
@ %bb.0:
	.save	{r11, lr}
	push	{r11, lr}
	.setfp	r11, sp
	mov	r11, sp
	.pad	#16
	sub	sp, sp, #16
	movw	r0, #0
	str	r0, [r11, #-4]
	movw	r0, #1
	str	r0, [sp]
	ldr	r0, [sp, #8]
	bl	scan
	ldr	r0, [sp, #4]
	bl	scan
.LBB2_1:                                @ =>This Inner Loop Header: Depth=1
	ldr	r0, [sp, #8]
	cmp	r0, #0
	ble	.LBB2_5
@ %bb.2:                                @   in Loop: Header=BB2_1 Depth=1
	ldr	r0, [sp, #8]
	add	r0, r0, r0, lsr #31
	asr	r1, r0, #1
	bic	r0, r0, #1
	ldr	r2, [sp, #8]
	cmp	r0, r2
	beq	.LBB2_4
@ %bb.3:                                @   in Loop: Header=BB2_1 Depth=1
	ldr	r0, [sp]
	ldr	r1, [sp, #4]
	mul	r0, r0, r1
	str	r0, [sp]
.LBB2_4:                                @   in Loop: Header=BB2_1 Depth=1
	ldr	r0, [sp, #4]
	ldr	r1, [sp, #4]
	mul	r0, r0, r1
	str	r0, [sp, #4]
	ldr	r0, [sp, #8]
	movw	r1, #2
	bl	__aeabi_idiv
	str	r0, [sp, #8]
	b	.LBB2_1
.LBB2_5:
	ldr	r0, [sp]
	bl	print
	movw	r0, #0
	mov	sp, r11
	pop	{r11, pc}
.Lfunc_end2:
	.size	main, .Lfunc_end2-main
	.cantunwind
	.fnend
                                        @ -- End function
	.type	.L.str,%object          @ @.str
	.section	.rodata.str1.1,"aMS",%progbits,1
.L.str:
	.asciz	"%d"
	.size	.L.str, 3

	.ident	"clang version 10.0.0 "
	.section	".note.GNU-stack","",%progbits
	.addrsig
	.addrsig_sym scan
	.addrsig_sym printf
	.addrsig_sym print
	.eabi_attribute	30, 6	@ Tag_ABI_optimization_goals
