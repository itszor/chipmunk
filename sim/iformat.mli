(* Overcomplicated internal-rep interface *)

type aluopcode2 = Mov | Not
and aluopcode3 =
    Lsl
  | Lsr
  | Asr
  | Ror
  | And
  | Ior
  | Eor
  | Bic
  | Add
  | Sub
  | Rsb
  | Mul
  | Div
  | Udiv
  | Mod
  | Umod
  | Mlh
  | Umlh
and alufopcode2 = Movf | Negf | Absf | Sqrf
and alufopcode3 = Addf | Subf | Mulf | Divf
and mmemfopcode = Ldmf | Stmf
and cond =
    Eq
  | Ne
  | Ge
  | Geu
  | Gt
  | Gtu
  | Le
  | Leu
  | Lt
  | Ltu
  | Andl
  | Eorl
  | Nandl
  | Neorl
and tcond = OnZero | OnNonzero
and ldtopcode = Ldx | Ldy | Ldz
and width = Byte | Halfword | Word
and life = Alive | Dead
and isimm = Immediate | Indirect
and srcreg = Reg of int * life
and srcfreg = Freg of int * life
and destreg = Dreg of int
and destfreg = Dfreg of int
and immreg = Bits of int | Srcreg of srcreg
and replace = Replace | Merge
and fill = Ones | Zeros
and extend = Signext | Zeroext
and part = Hi | Lo
and volatile = Volatile | Nonvolatile
and precision = Single | Double
and up = IncAfter | DecBefore
and writebk = Writeback | Discard
and shft = SpliceShift | Splice
and format =
    Alu2F of aluopcode2 * destreg * srcreg
  | Alu2iF of aluopcode2 * destreg * int32
  | Alu3F of aluopcode3 * destreg * srcreg * srcreg
  | Alu3iF of aluopcode3 * destreg * srcreg * int32
  | Alu2fF of alufopcode2 * precision * destfreg * srcfreg
  | Alu2fiF of alufopcode2 * precision * destfreg * int32
  | Alu3fF of alufopcode3 * precision * destfreg * srcfreg * srcfreg
  | Alu3fiF of alufopcode3 * precision * destfreg * srcfreg * int32
  | FltF of precision * destfreg * srcreg
  | FixF of precision * destreg * srcfreg
  | CmpF of cond * destreg * srcreg * srcreg
  | CmpiF of cond * destreg * srcreg * int32
  | CmpfF of cond * precision * destreg * srcfreg * srcfreg
  | MvcF of destreg * int32 * fill * replace * part
  | LdrF of destreg * srcreg * srcreg * volatile * width
  | LdriF of destreg * srcreg * int32 * volatile * width
  | StrF of srcreg * srcreg * srcreg * volatile * width
  | StriF of srcreg * srcreg * int32 * volatile * width
  | LdfF of destfreg * srcreg * srcreg * volatile * precision
  | LdfiF of destfreg * srcreg * int32 * volatile * precision
  | StfF of srcfreg * srcreg * srcreg * volatile * precision
  | StfiF of srcfreg * srcreg * int32 * volatile * precision
  | BfxF of destreg * srcreg * int * int * extend
  | SplF of destreg * srcreg * srcreg * shft * int
  | LdmF of destreg * destreg array * up * writebk
  | StmF of destreg * srcreg array * up * writebk
  | LdmfF of destreg * destfreg array * up * writebk
  | StmfF of destreg * srcfreg array * up * writebk
  | SwiF of int32
  | LdtF of ldtopcode * int32
  | TrapF of tcond * srcreg * immreg
  | CbrF of srcreg * immreg * immreg
  | CallF of immreg * immreg
  | JumpF of immreg
  | RetF
  | NoopF

val decodeArray : (int32 -> format) array
