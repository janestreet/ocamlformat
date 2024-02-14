(* Legacy immediate attributes are rewritten into layout annotations *)

type old_imm [@@immediate]
type old_imm64 [@@immediate64]

type old_imm = p [@@immediate]
type old_imm64 = q [@@immediate64]

(* Comments are not dropped *)

type old_imm (* a *)[@@immediate(* b *)](* c *)

type old_imm64 = (* a *)s (* b *) [@@immediate64](* c *)

(* Do nothing if there are more than one attribute *)

type old_imm [@@immediate] [@@immediate]
type old_imm [@@immediate] [@@immediate64]

type old_imm64 [@@immediate64] [@@immediate64]
type old_imm64 [@@immediate64] [@@immediate]

(* Do nothing if there's already a layout annotation *)

type old_imm: immediate [@@immediate]
type old_imm: immediate64 [@@immediate]

type old_imm64: immediate64 [@@immediate64]
type old_imm64: immediate [@@immediate64]
