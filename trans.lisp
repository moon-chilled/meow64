(cl:in-package #:meow64)

#|
Code translation

register mappings:
eax	eax
ecx	ecx
edx	edx
ebx	ebx
esp	r13d
ebp	ebp
esi	r14d
edi	r15d

This has the following desirable properties:

- Caller-saved registers are mapped to caller-saved registers, and callee-saved registers to callee-saved registers
- Registers with high halves are mapped to registers with high halves
- The registers which most commonly receive special treatment (eax/edx/ecx for shifts, multiplies, divides, cas, some short instruction forms, and probably other things I'm forgetting) by both the isas are mapped to themselves
  - esi and edi are not identity-mapped for string ops, but string ops are fairly rare, and have a high enough startup cost that the extra movs to put them in place are irrelevant

Esp can't be identity-mapped, because rsp is needed for the return stack.  Identity-mapping esi/edi would save on REX prefixes, but I don't think it's worth the overhead on thunks.

Possibly, the mappings of esp and ebp should be swapped; it depends on which one gets more use, as whatever is mapped to dst ebp gets to skip rex prefixes.

Dst rsp is the return stack, and it comprises a sequence of 2-word records.  At any given point in time, qword [rsp] is the dst return address, and qword [rsp+8] is a zero-extended src return address.  Then, the call sequence looks like this:

sub	r13d, 4
mov	dword [r13d], src-return-address
push	src-return-address
call	target
add	rsp, 8

And the return sequence looks like this:

mov	edi, [r13d]
add	r13d, 4
cmp	edi, [rsp+8]
jne	bail
ret

src-return-address actually corresponds to the dst address _after_ 'add rsp,8', bail is responsible for cleaning up rsp properly, so we should never get out of sync with the hardware ras.  (We will need to induce hardware return predict failure when we encounter it, though.)

Because RSP is a sequence of 16-byte records, we can trivially keep it 16-byte-aligned, and so do not need to fix it up in thunks.
|#
