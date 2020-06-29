/* Apply-hack.x86-64.S
;
; Copyright (c) 2008-2015, The CHICKEN Team
; Copyright (c) 2007, Felix L. Winkelmann
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
; conditions are met:
;
;   Redistributions of source code must retain the above copyright notice, this list of conditions and the following
;     disclaimer. 
;   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
;     disclaimer in the documentation and/or other materials provided with the distribution. 
;   Neither the name of the author nor the names of its contributors may be used to endorse or promote
;     products derived from this software without specific prior written permission. 
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
; AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.
*/

	.text

	.globl _C_do_apply_hack
#ifndef __APPLE__
	.type _C_do_apply_hack, @function
#endif

_C_do_apply_hack:
	subq $8, %rsp		/* force non-16 byte alignment */
	movq %rdi, %r11		/* get proc */
	movq %rsi, %r10		/* save buffer address, before we clobber %rsi */
	cmpl $6, %edx		/* clamp at 6 */
	ja l2
	je l3
	movq $6, %rbx		/* (6 - count) * 4 gives instruction address */
	subq %rdx, %rbx
	shlq $2, %rbx
	lea l3(%rip), %rdx
	addq %rdx, %rbx
	jmp *%rbx
l2:	lea 48(%r10), %rsp	/* %r10 must be 16-byte aligned at this point */
l3:	movq 40(%r10), %r9      /* fill registers... */
	movq 32(%r10), %r8
	movq 24(%r10), %rcx
	movq 16(%r10), %rdx
	movq 8(%r10), %rsi
	movq (%r10), %rdi
	xorq %rax, %rax
	call *%r11

/* Set non-executable stack for Linux ELF target */
#if defined(__ELF__)
	.section .note.GNU-stack,"",%progbits
#endif
