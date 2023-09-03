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

ras	rsp
base	r12
scratch	r11 r10 r9 r8 rdi rsi

This has the following desirable properties:

- Caller-saved registers are mapped to caller-saved registers, and callee-saved registers to callee-saved registers
- Registers with high halves are mapped to registers with high halves
- The registers which most commonly receive special treatment (eax/edx/ecx for shifts, multiplies, divides, cas, some short instruction forms, and probably other things I'm forgetting) by both the isas are mapped to themselves
  - esi and edi are not identity-mapped for string ops, but actually (e.g.) src edi is relative to r12, whereas stosb wants to write to absolute dst rdi, so that wouldn't work anyway
  - since rsi/rdi are scratch clobbering them for strings ops is trivial

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

Because rsp is a sequence of 16-byte records, we can trivially keep it 16-byte-aligned, and so do not need to fix it up in thunks.

Pointers need to fit in 32 bits, but MAP_32BIT is borked, so we reserve 4gb of address space and point dst r12 at the base of it.
A decent chunk at the start is unmapped to catch null accesses, and the page _before_ the start is for global state (unless there's a bug in the translator, a buggy application should never be possible to form an access thereto).

Dst rsp is an 8mb-aligned chunk, but the highest two words are reserved
High word stores a copy of the memory base
Low word is split: high 32 bits is a tls pointer (offset into membase), low is a stashed copy of src esp
Callback trampoline has to retrieve memory base and esp; if trampoline callee might call a callback, then stash esp appropriately; enables recursive calls between 32- and 64-bit code
Sequence:
mov	scratch, rsp
and	scratch, -8mb
add	scratch, 8mb
ld/st	[scratch - 16]
For efficient operation on arm; for native amd64, would be better to just load from [scratch + 8mb - 16] or w/e; can be a hint maybe (for potential eventual linux version...)

I really want to do function pointers the way wasm does, but I'm sure that would break somebody, so binary search (possibly w/inline cache) it is :\
|#
