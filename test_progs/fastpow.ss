
a.out:     file format elf32-littlearm


Disassembly of section .init:

00000380 <_init>:
 380:	e92d4008 	push	{r3, lr}
 384:	eb000027 	bl	428 <call_weak_fn>
 388:	e8bd8008 	pop	{r3, pc}

Disassembly of section .plt:

0000038c <.plt>:
 38c:	e52de004 	push	{lr}		; (str lr, [sp, #-4]!)
 390:	e59fe004 	ldr	lr, [pc, #4]	; 39c <.plt+0x10>
 394:	e08fe00e 	add	lr, pc, lr
 398:	e5bef008 	ldr	pc, [lr, #8]!
 39c:	00010c64 	.word	0x00010c64

000003a0 <__cxa_finalize@plt>:
 3a0:	e28fc600 	add	ip, pc, #0, 12
 3a4:	e28cca10 	add	ip, ip, #16, 20	; 0x10000
 3a8:	e5bcfc64 	ldr	pc, [ip, #3172]!	; 0xc64

000003ac <__libc_start_main@plt>:
 3ac:	e28fc600 	add	ip, pc, #0, 12
 3b0:	e28cca10 	add	ip, ip, #16, 20	; 0x10000
 3b4:	e5bcfc5c 	ldr	pc, [ip, #3164]!	; 0xc5c

000003b8 <__gmon_start__@plt>:
 3b8:	e28fc600 	add	ip, pc, #0, 12
 3bc:	e28cca10 	add	ip, ip, #16, 20	; 0x10000
 3c0:	e5bcfc54 	ldr	pc, [ip, #3156]!	; 0xc54

000003c4 <abort@plt>:
 3c4:	e28fc600 	add	ip, pc, #0, 12
 3c8:	e28cca10 	add	ip, ip, #16, 20	; 0x10000
 3cc:	e5bcfc4c 	ldr	pc, [ip, #3148]!	; 0xc4c

Disassembly of section .text:

000003d0 <_start>:
 3d0:	e3a0b000 	mov	fp, #0
 3d4:	e3a0e000 	mov	lr, #0
 3d8:	e49d1004 	pop	{r1}		; (ldr r1, [sp], #4)
 3dc:	e1a0200d 	mov	r2, sp
 3e0:	e52d2004 	push	{r2}		; (str r2, [sp, #-4]!)
 3e4:	e52d0004 	push	{r0}		; (str r0, [sp, #-4]!)
 3e8:	e59fa028 	ldr	sl, [pc, #40]	; 418 <_start+0x48>
 3ec:	e28f3024 	add	r3, pc, #36	; 0x24
 3f0:	e08aa003 	add	sl, sl, r3
 3f4:	e59fc020 	ldr	ip, [pc, #32]	; 41c <_start+0x4c>
 3f8:	e79ac00c 	ldr	ip, [sl, ip]
 3fc:	e52dc004 	push	{ip}		; (str ip, [sp, #-4]!)
 400:	e59f3018 	ldr	r3, [pc, #24]	; 420 <_start+0x50>
 404:	e79a3003 	ldr	r3, [sl, r3]
 408:	e59f0014 	ldr	r0, [pc, #20]	; 424 <_start+0x54>
 40c:	e79a0000 	ldr	r0, [sl, r0]
 410:	ebffffe5 	bl	3ac <__libc_start_main@plt>
 414:	ebffffea 	bl	3c4 <abort@plt>
 418:	00010be8 	.word	0x00010be8
 41c:	0000001c 	.word	0x0000001c
 420:	0000002c 	.word	0x0000002c
 424:	00000030 	.word	0x00000030

00000428 <call_weak_fn>:
 428:	e59f3014 	ldr	r3, [pc, #20]	; 444 <call_weak_fn+0x1c>
 42c:	e59f2014 	ldr	r2, [pc, #20]	; 448 <call_weak_fn+0x20>
 430:	e08f3003 	add	r3, pc, r3
 434:	e7932002 	ldr	r2, [r3, r2]
 438:	e3520000 	cmp	r2, #0
 43c:	012fff1e 	bxeq	lr
 440:	eaffffdc 	b	3b8 <__gmon_start__@plt>
 444:	00010bc8 	.word	0x00010bc8
 448:	00000028 	.word	0x00000028

0000044c <deregister_tm_clones>:
 44c:	e59f002c 	ldr	r0, [pc, #44]	; 480 <deregister_tm_clones+0x34>
 450:	e59f302c 	ldr	r3, [pc, #44]	; 484 <deregister_tm_clones+0x38>
 454:	e08f0000 	add	r0, pc, r0
 458:	e08f3003 	add	r3, pc, r3
 45c:	e1530000 	cmp	r3, r0
 460:	e59f3020 	ldr	r3, [pc, #32]	; 488 <deregister_tm_clones+0x3c>
 464:	e08f3003 	add	r3, pc, r3
 468:	012fff1e 	bxeq	lr
 46c:	e59f2018 	ldr	r2, [pc, #24]	; 48c <deregister_tm_clones+0x40>
 470:	e7933002 	ldr	r3, [r3, r2]
 474:	e3530000 	cmp	r3, #0
 478:	012fff1e 	bxeq	lr
 47c:	e12fff13 	bx	r3
 480:	00010be4 	.word	0x00010be4
 484:	00010be0 	.word	0x00010be0
 488:	00010b94 	.word	0x00010b94
 48c:	00000024 	.word	0x00000024

00000490 <register_tm_clones>:
 490:	e59f0038 	ldr	r0, [pc, #56]	; 4d0 <register_tm_clones+0x40>
 494:	e59f1038 	ldr	r1, [pc, #56]	; 4d4 <register_tm_clones+0x44>
 498:	e08f0000 	add	r0, pc, r0
 49c:	e08f1001 	add	r1, pc, r1
 4a0:	e0411000 	sub	r1, r1, r0
 4a4:	e59f202c 	ldr	r2, [pc, #44]	; 4d8 <register_tm_clones+0x48>
 4a8:	e1a03fa1 	lsr	r3, r1, #31
 4ac:	e0831141 	add	r1, r3, r1, asr #2
 4b0:	e08f2002 	add	r2, pc, r2
 4b4:	e1b010c1 	asrs	r1, r1, #1
 4b8:	012fff1e 	bxeq	lr
 4bc:	e59f3018 	ldr	r3, [pc, #24]	; 4dc <register_tm_clones+0x4c>
 4c0:	e7923003 	ldr	r3, [r2, r3]
 4c4:	e3530000 	cmp	r3, #0
 4c8:	012fff1e 	bxeq	lr
 4cc:	e12fff13 	bx	r3
 4d0:	00010ba0 	.word	0x00010ba0
 4d4:	00010b9c 	.word	0x00010b9c
 4d8:	00010b48 	.word	0x00010b48
 4dc:	00000034 	.word	0x00000034

000004e0 <__do_global_dtors_aux>:
 4e0:	e59f304c 	ldr	r3, [pc, #76]	; 534 <__do_global_dtors_aux+0x54>
 4e4:	e59f204c 	ldr	r2, [pc, #76]	; 538 <__do_global_dtors_aux+0x58>
 4e8:	e08f3003 	add	r3, pc, r3
 4ec:	e08f2002 	add	r2, pc, r2
 4f0:	e5d33000 	ldrb	r3, [r3]
 4f4:	e3530000 	cmp	r3, #0
 4f8:	112fff1e 	bxne	lr
 4fc:	e59f3038 	ldr	r3, [pc, #56]	; 53c <__do_global_dtors_aux+0x5c>
 500:	e92d4010 	push	{r4, lr}
 504:	e7923003 	ldr	r3, [r2, r3]
 508:	e3530000 	cmp	r3, #0
 50c:	0a000002 	beq	51c <__do_global_dtors_aux+0x3c>
 510:	e59f3028 	ldr	r3, [pc, #40]	; 540 <__do_global_dtors_aux+0x60>
 514:	e79f0003 	ldr	r0, [pc, r3]
 518:	ebffffa0 	bl	3a0 <__cxa_finalize@plt>
 51c:	ebffffca 	bl	44c <deregister_tm_clones>
 520:	e59f301c 	ldr	r3, [pc, #28]	; 544 <__do_global_dtors_aux+0x64>
 524:	e3a02001 	mov	r2, #1
 528:	e08f3003 	add	r3, pc, r3
 52c:	e5c32000 	strb	r2, [r3]
 530:	e8bd8010 	pop	{r4, pc}
 534:	00010b50 	.word	0x00010b50
 538:	00010b0c 	.word	0x00010b0c
 53c:	00000020 	.word	0x00000020
 540:	00010b20 	.word	0x00010b20
 544:	00010b10 	.word	0x00010b10

00000548 <frame_dummy>:
 548:	eaffffd0 	b	490 <register_tm_clones>

0000054c <scan>:
 54c:	e52db004 	push	{fp}		; (str fp, [sp, #-4]!)
 550:	e28db000 	add	fp, sp, #0
 554:	e24dd00c 	sub	sp, sp, #12
 558:	e50b0008 	str	r0, [fp, #-8]
 55c:	e51b3008 	ldr	r3, [fp, #-8]
 560:	e2833001 	add	r3, r3, #1
 564:	e50b3008 	str	r3, [fp, #-8]
 568:	e1a00000 	nop			; (mov r0, r0)
 56c:	e28bd000 	add	sp, fp, #0
 570:	e49db004 	pop	{fp}		; (ldr fp, [sp], #4)
 574:	e12fff1e 	bx	lr

00000578 <print>:
 578:	e52db004 	push	{fp}		; (str fp, [sp, #-4]!)
 57c:	e28db000 	add	fp, sp, #0
 580:	e24dd00c 	sub	sp, sp, #12
 584:	e50b0008 	str	r0, [fp, #-8]
 588:	e51b3008 	ldr	r3, [fp, #-8]
 58c:	e2833001 	add	r3, r3, #1
 590:	e50b3008 	str	r3, [fp, #-8]
 594:	e1a00000 	nop			; (mov r0, r0)
 598:	e28bd000 	add	sp, fp, #0
 59c:	e49db004 	pop	{fp}		; (ldr fp, [sp], #4)
 5a0:	e12fff1e 	bx	lr

000005a4 <main>:
 5a4:	e92d4800 	push	{fp, lr}
 5a8:	e28db004 	add	fp, sp, #4
 5ac:	e24dd010 	sub	sp, sp, #16
 5b0:	e3a03001 	mov	r3, #1
 5b4:	e50b3008 	str	r3, [fp, #-8]
 5b8:	e51b0010 	ldr	r0, [fp, #-16]
 5bc:	ebffffe2 	bl	54c <scan>
 5c0:	e51b000c 	ldr	r0, [fp, #-12]
 5c4:	ebffffe0 	bl	54c <scan>
 5c8:	ea000013 	b	61c <main+0x78>
 5cc:	e51b3010 	ldr	r3, [fp, #-16]
 5d0:	e1a02fa3 	lsr	r2, r3, #31
 5d4:	e0823003 	add	r3, r2, r3
 5d8:	e1a030c3 	asr	r3, r3, #1
 5dc:	e1a03083 	lsl	r3, r3, #1
 5e0:	e51b2010 	ldr	r2, [fp, #-16]
 5e4:	e1520003 	cmp	r2, r3
 5e8:	0a000003 	beq	5fc <main+0x58>
 5ec:	e51b3008 	ldr	r3, [fp, #-8]
 5f0:	e51b200c 	ldr	r2, [fp, #-12]
 5f4:	e0030392 	mul	r3, r2, r3
 5f8:	e50b3008 	str	r3, [fp, #-8]
 5fc:	e51b300c 	ldr	r3, [fp, #-12]
 600:	e0030393 	mul	r3, r3, r3
 604:	e50b300c 	str	r3, [fp, #-12]
 608:	e51b3010 	ldr	r3, [fp, #-16]
 60c:	e1a02fa3 	lsr	r2, r3, #31
 610:	e0823003 	add	r3, r2, r3
 614:	e1a030c3 	asr	r3, r3, #1
 618:	e50b3010 	str	r3, [fp, #-16]
 61c:	e51b3010 	ldr	r3, [fp, #-16]
 620:	e3530000 	cmp	r3, #0
 624:	caffffe8 	bgt	5cc <main+0x28>
 628:	e51b0008 	ldr	r0, [fp, #-8]
 62c:	ebffffd1 	bl	578 <print>
 630:	e3a03000 	mov	r3, #0
 634:	e1a00003 	mov	r0, r3
 638:	e24bd004 	sub	sp, fp, #4
 63c:	e8bd8800 	pop	{fp, pc}

00000640 <__libc_csu_init>:
 640:	e92d47f0 	push	{r4, r5, r6, r7, r8, r9, sl, lr}
 644:	e1a07000 	mov	r7, r0
 648:	e59f6048 	ldr	r6, [pc, #72]	; 698 <__libc_csu_init+0x58>
 64c:	e59f5048 	ldr	r5, [pc, #72]	; 69c <__libc_csu_init+0x5c>
 650:	e08f6006 	add	r6, pc, r6
 654:	e08f5005 	add	r5, pc, r5
 658:	e0466005 	sub	r6, r6, r5
 65c:	e1a08001 	mov	r8, r1
 660:	e1a09002 	mov	r9, r2
 664:	ebffff45 	bl	380 <_init>
 668:	e1b06146 	asrs	r6, r6, #2
 66c:	08bd87f0 	popeq	{r4, r5, r6, r7, r8, r9, sl, pc}
 670:	e3a04000 	mov	r4, #0
 674:	e2844001 	add	r4, r4, #1
 678:	e4953004 	ldr	r3, [r5], #4
 67c:	e1a02009 	mov	r2, r9
 680:	e1a01008 	mov	r1, r8
 684:	e1a00007 	mov	r0, r7
 688:	e12fff33 	blx	r3
 68c:	e1560004 	cmp	r6, r4
 690:	1afffff7 	bne	674 <__libc_csu_init+0x34>
 694:	e8bd87f0 	pop	{r4, r5, r6, r7, r8, r9, sl, pc}
 698:	000108b4 	.word	0x000108b4
 69c:	000108ac 	.word	0x000108ac

000006a0 <__libc_csu_fini>:
 6a0:	e12fff1e 	bx	lr

Disassembly of section .fini:

000006a4 <_fini>:
 6a4:	e92d4008 	push	{r3, lr}
 6a8:	e8bd8008 	pop	{r3, pc}
