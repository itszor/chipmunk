;;- Machine description for the Chameleon architecture.

;; This file is part of GNU CC.

;; GNU CC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU CC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU CC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; attributes

(define_attr "length" "" (const_int 4))

;; constants

(define_constants [
  (UNSPEC_SPL 0)
  (UNSPEC_SPLS 1)
])

(define_insn "nop"
  [(const_int 0)]
  ""
  "mov r0,r0")

(define_expand "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
        (match_operand:SI 1 "general_operand" ""))]
  ""
  "{
    if (GET_CODE(operands[0]) == MEM)
    {
      operands[1] = force_reg(SImode, operands[1]);
    }
  }")

(define_insn "*movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r,m,r,r,f,f")
        (match_operand:SI 1 "general_operand"      " r,I,m,r,M,i,f,m"))]
  "register_operand (operands[0], SImode)
   || register_operand (operands[1], SImode)"
  "@
  mov %0,%E1
  mov %0,#%d1
  ldr.w %0,%1
  str.w %E1,%0
  mvc.%e1 %0,#%d1
  #
  movf.d %0,%E1
  ldf.s %0,%1"
 [(set_attr "length" "4,4,4,4,4,8,4,4")])

(define_split 
  [(set (match_operand:SI 0 "register_operand" "")
        (match_operand:SI 1 "multi_insn_const_operand" ""))]
  "multi_insn_const_operand (operands[1], SImode)"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 0) (ior:SI (and:SI (match_dup 0) (const_int 65535))
                              (and:SI (match_dup 2) (const_int -65536))))]
  "{
    if (GET_CODE (operands[1]) == SYMBOL_REF)
      {
        emit_insn (gen_mvc_sym_lo (operands[0], operands[1]));
        emit_insn (gen_mvch (operands[0], operands[0], operands[1]));
        DONE;
      }
    else
      {
        unsigned int val = INTVAL(operands[1]);
        operands[1] = GEN_INT(val & 0x0000ffff);
        operands[2] = GEN_INT(val & 0xffff0000);
      }
  }")

; 'K' is a low-order 16-bit constant.
(define_insn "mvcl"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ior:SI (and:SI (match_operand:SI 1 "register_operand" "0")
                        (const_int -65536))
                (and:SI (match_operand:SI 2 "symbol_or_const_operand" "Ks")
                        (const_int 65535))))]
  ""
  "mvc.l %0,#%2")

; 'L' is a high-order 16-bit constant.
(define_insn "mvch"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ior:SI (and:SI (match_operand:SI 1 "register_operand" "0")
                        (const_int 65535))
                (and:SI (match_operand:SI 2 "symbol_or_const_operand" "Ls")
                        (const_int -65536))))]
  ""
  "mvc.h %0,#%2")

(define_insn "mvc_sym_lo"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (and:SI (match_operand:SI 1 "symbol_operand" "s")
                (const_int 65535)))]
  ""
  "mvc.el %0,#%1")

(define_insn "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,r,m,r")
        (match_operand:QI 1 "general_operand" "r,I,m,r,J"))]
  ""
  "@
  mov %0,%E1
  mov %0,#%d1
  ldr.b %0,%1
  str.b %E1,%0
  mov %0,#%k1")

(define_insn "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r,r,m")
        (match_operand:HI 1 "general_operand" "r,I,M,m,r"))]
  ""
  "@
  mov %0,%E1
  mov %0,#%d1
  mvc.%e1 %0,#%d1
  ldr.h %0,%1
  str.h %E1,%0")

(define_expand "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "")
        (match_operand:DI 1 "general_operand" ""))]
  "reload_completed"
{
  dop_move_double (operands[0], operands[1]);
  DONE;
})

(define_expand "movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "")
        (match_operand:SF 1 "general_operand" ""))]
  ""
  "{
    if (GET_CODE(operands[0]) == MEM)
    {
      operands[1] = force_reg(SFmode, operands[1]);
    }
  }")

(define_insn "*movsf_insn"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=f,f,r,f,m,r,m")
        (match_operand:SF 1 "general_operand"      " f,G,r,m,f,m,r"))]
  ""
  "@
  movf.s %0,%E1
  movf.s %0,#%G1
  mov %0,%1
  ldf.s %0,%1
  stf.s %E1,%0
  ldr.w %0,%1
  str.w %E1,%0")

/* The 'V' constraint is a plain dereferenced register, with no offset */
(define_insn_and_split "movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=f,f,f,m,r,m")
        (match_operand:DF 1 "general_operand"      " f,G,m,f,m,r"))]
  ""
  "@
  movf.d %0,%E1
  movf.d %0,#%G1
  ldf.d %0,%1
  stf.d %E1,%0
  #
  #"
  "reload_completed"
  [(const_int 0)]
{
  /* If we have a DFmode value in general registers, we should split it
     here.  */
  if ((GET_CODE (operands[0]) == REG
       && REGNO_REG_CLASS (REGNO (operands[0])) == GENERAL_REGS)
      || (GET_CODE (operands[1]) == REG
          && REGNO_REG_CLASS (REGNO (operands[1])) == GENERAL_REGS))
    {
      dop_move_double (operands[0], operands[1]);
      DONE;
    }
  else
    FAIL;
})

(define_expand "load_multiple"
  [(match_par_dup 3 [(set (match_operand:SI 0 "" "")
                          (match_operand:SI 1 "" ""))
                     (use (match_operand:SI 2 "" ""))])]
  ""
  "
  /* fprintf(stderr, \"Trying load_multiple expansion\n\"); */

  if (GET_CODE(operands[2]) != CONST_INT
      || INTVAL(operands[2]) > 3
      || GET_CODE(operands[1]) != MEM
      || GET_CODE(operands[0]) != REG)
    FAIL;
  
  operands[3]
    = dop_gen_load_multiple (REGNO(operands[0]), INTVAL (operands[2]),
                             force_reg (SImode, XEXP (operands[1], 0)),
                             TRUE, FALSE, RTX_UNCHANGING_P(operands[1]),
                             MEM_IN_STRUCT_P(operands[1]),
                             MEM_SCALAR_P(operands[1]));
  ")
                             

/* post-increment operations */

(define_insn "*ldmsi1_postinc_wb"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "register_operand" "=t")
          (plus:SI (match_operand:SI 2 "register_operand" "1")
                   (const_int 4)))
     (set (match_operand:SI 3 "register_operand" "=r")
          (mem:SI (match_dup 2)))])]
  "XVECLEN (operands[0], 0) == 2"
  "ldm.i %1*,[%3]")

(define_insn "*ldmsi2_postinc_wb"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "register_operand" "=t")
          (plus:SI (match_operand:SI 2 "register_operand" "1")
                   (const_int 8)))
     (set (match_operand:SI 3 "register_operand" "=r")
          (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "=r")
          (mem:SI (plus:SI (match_dup 2) (const_int 4))))])]
  "XVECLEN (operands[0], 0) == 3"
  "ldm.i %1*,[%3, %4]")

(define_insn "*ldmsi3_postinc_wb"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "register_operand" "=t")
          (plus:SI (match_operand:SI 2 "register_operand" "1")
                   (const_int 12)))
     (set (match_operand:SI 3 "register_operand" "=r")
          (mem:SI (match_dup 2)))
     (set (match_operand:SI 4 "register_operand" "=r")
          (mem:SI (plus:SI (match_dup 2) (const_int 4))))
     (set (match_operand:SI 5 "register_operand" "=r")
          (mem:SI (plus:SI (match_dup 2) (const_int 8))))])]
  "XVECLEN (operands[0], 0) == 4"
  "ldm.i %1*,[%3, %4, %5]")

(define_insn "*ldmsi2_postinc"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 2 "register_operand" "=r")
          (mem:SI (match_operand:SI 1 "register_operand" "t")))
     (set (match_operand:SI 3 "register_operand" "=r")
          (mem:SI (plus:SI (match_dup 1) (const_int 4))))])]
  "XVECLEN (operands[0], 0) == 2"
  "ldm.i %1,[%2, %3]")

(define_insn "*ldmsi3_postinc"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 2 "register_operand" "=r")
          (mem:SI (match_operand:SI 1 "register_operand" "t")))
     (set (match_operand:SI 3 "register_operand" "=r")
          (mem:SI (plus:SI (match_dup 1) (const_int 4))))
     (set (match_operand:SI 4 "register_operand" "=r")
          (mem:SI (plus:SI (match_dup 1) (const_int 8))))])]
  "XVECLEN (operands[0], 0) == 3"
  "ldm.i %1,[%2, %3, %4]")

/*
(define_insn "*stmsi1_postinc_wb"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 1 "register_operand" "=t")
          (plus:SI (match_operand:SI 2 "register_operand" "1")
                   (const_int 4)))
     (set (mem:SI (match_dup 2))
          (match_operand:SI 3 "register_operand" "r"))])]
  ""
  "stm.i %1*,[%3]")

(define_insn "*stmsi2_postinc_wb"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 1 "register_operand" "=t")
          (plus:SI (match_operand:SI 2 "register_operand" "1")
                   (const_int 8)))
     (set (mem:SI (match_dup 2))
          (match_operand:SI 3 "register_operand" "r"))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
          (match_operand:SI 4 "register_operand" "r"))])]
  ""
  "stm.i %1*,[%3, %4]")

(define_insn "*stmsi3_postinc_wb"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 1 "register_operand" "=t")
          (plus:SI (match_operand:SI 2 "register_operand" "1")
                   (const_int 12)))
     (set (mem:SI (match_dup 2))
          (match_operand:SI 3 "register_operand" "r"))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
          (match_operand:SI 4 "register_operand" "r"))
     (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
          (match_operand:SI 5 "register_operand" "r"))])]
  ""
  "stm.i %1*,[%3, %4, %5]")

(define_insn "*stmsi1_postinc"
  [(match_parallel 0 "store_multiple_operation"
    [(set (mem:SI (match_operand:SI 1 "register_operand" "t"))
          (match_operand:SI 2 "register_operand" "=r"))])]
  ""
  "stm.i %1,[%2]")

(define_insn "*stmsi2_postinc"
  [(match_parallel 0 "store_multiple_operation"
    [(set (mem:SI (match_operand:SI 1 "register_operand" "t"))
          (match_operand:SI 2 "register_operand" "=r"))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 4)))
          (match_operand:SI 3 "register_operand" "=r"))])]
  ""
  "stm.i %1,[%2, %3]")

(define_insn "*stmsi3_postinc"
  [(match_parallel 0 "store_multiple_operation"
    [(set (mem:SI (match_operand:SI 1 "register_operand" "t"))
          (match_operand:SI 2 "register_operand" "=r"))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 4)))
          (match_operand:SI 3 "register_operand" "=r"))
     (set (mem:SI (plus:SI (match_dup 1) (const_int 8)))
          (match_operand:SI 4 "register_operand" "=r"))])]
  ""
  "stm.i %1,[%2, %3, %4]")
*/

/* pre-decrement operations */

(define_insn "*ldmsi1_predec_wb"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "register_operand" "=t")
          (plus:SI (match_operand:SI 2 "register_operand" "1")
                   (const_int -4)))
     (set (match_operand:SI 3 "register_operand" "=r")
          (mem:SI (plus:SI (match_dup 2) (const_int -4))))])]
  "XVECLEN (operands[0], 0) == 2"
  "ldm.d %1*,[%3]")

(define_insn "*ldmsi2_predec_wb"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "register_operand" "=t")
          (plus:SI (match_operand:SI 2 "register_operand" "1")
                   (const_int -8)))
     (set (match_operand:SI 3 "register_operand" "=r")
          (mem:SI (plus:SI (match_dup 2) (const_int -4))))
     (set (match_operand:SI 4 "register_operand" "=r")
          (mem:SI (plus:SI (match_dup 2) (const_int -8))))])]
  "XVECLEN (operands[0], 0) == 3"
  "ldm.d %1*,[%3, %4]")

(define_insn "*ldmsi3_predec_wb"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "register_operand" "=t")
          (plus:SI (match_operand:SI 2 "register_operand" "1")
                   (const_int -12)))
     (set (match_operand:SI 3 "register_operand" "=r")
          (mem:SI (plus:SI (match_dup 2) (const_int -4))))
     (set (match_operand:SI 4 "register_operand" "=r")
          (mem:SI (plus:SI (match_dup 2) (const_int -8))))
     (set (match_operand:SI 5 "register_operand" "=r")
          (mem:SI (plus:SI (match_dup 2) (const_int -12))))])]
  "XVECLEN (operands[0], 0) == 4"
  "ldm.d %1*,[%3, %4, %5]")

(define_insn "*ldmsi2_predec"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 2 "register_operand" "=r")
          (mem:SI (plus:SI (match_operand:SI 1 "register_operand" "t")
                           (const_int -4))))
     (set (match_operand:SI 3 "register_operand" "=r")
          (mem:SI (plus:SI (match_dup 1) (const_int -8))))])]
  "XVECLEN (operands[0], 0) == 2"
  "ldm.d %1,[%2, %3]")

(define_insn "*ldmsi3_predec"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 2 "register_operand" "=r")
          (mem:SI (plus:SI (match_operand:SI 1 "register_operand" "t")
                           (const_int -4))))
     (set (match_operand:SI 3 "register_operand" "=r")
          (mem:SI (plus:SI (match_dup 1) (const_int -8))))
     (set (match_operand:SI 4 "register_operand" "=r")
          (mem:SI (plus:SI (match_dup 1) (const_int -12))))])]
  "XVECLEN (operands[0], 0) == 3"
  "ldm.d %1,[%2, %3, %4]")

/*
(define_insn "*stmsi1_predec_wb"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 1 "register_operand" "=t")
          (plus:SI (match_operand:SI 2 "register_operand" "1")
                   (const_int -4)))
     (set (mem:SI (plus:SI (match_dup 2) (const_int -4)))
          (match_operand:SI 3 "register_operand" "r"))])]
  ""
  "stm.d %1*,[%3]")

(define_insn "*stmsi2_predec_wb"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 1 "register_operand" "=t")
          (plus:SI (match_operand:SI 2 "register_operand" "1")
                   (const_int -8)))
     (set (mem:SI (plus:SI (match_dup 2) (const_int -4)))
          (match_operand:SI 3 "register_operand" "r"))
     (set (mem:SI (plus:SI (match_dup 2) (const_int -8)))
          (match_operand:SI 4 "register_operand" "r"))])]
  ""
  "stm.d %1*,[%3, %4]")

(define_insn "*stmsi3_predec_wb"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 1 "register_operand" "=t")
          (plus:SI (match_operand:SI 2 "register_operand" "1")
                   (const_int -12)))
     (set (mem:SI (plus:SI (match_dup 2) (const_int -4)))
          (match_operand:SI 3 "register_operand" "r"))
     (set (mem:SI (plus:SI (match_dup 2) (const_int -8)))
          (match_operand:SI 4 "register_operand" "r"))
     (set (mem:SI (plus:SI (match_dup 2) (const_int -12)))
          (match_operand:SI 5 "register_operand" "r"))])]
  ""
  "stm.d %1*,[%3, %4, %5]")

(define_insn "*stmsi1_predec"
  [(match_parallel 0 "store_multiple_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 1 "register_operand" "t")
                           (const_int -4)))
          (match_operand:SI 2 "register_operand" "=r"))])]
  ""
  "stm.d %1,[%2]")

(define_insn "*stmsi2_predec"
  [(match_parallel 0 "store_multiple_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 1 "register_operand" "t")
                           (const_int -4)))
          (match_operand:SI 2 "register_operand" "=r"))
     (set (mem:SI (plus:SI (match_dup 1) (const_int -8)))
          (match_operand:SI 3 "register_operand" "=r"))])]
  ""
  "stm.d %1,[%2, %3]")

(define_insn "*stmsi3_predec"
  [(match_parallel 0 "store_multiple_operation"
    [(set (mem:SI (plus:SI (match_operand:SI 1 "register_operand" "t")
                           (const_int -4)))
          (match_operand:SI 2 "register_operand" "=r"))
     (set (mem:SI (plus:SI (match_dup 1) (const_int -8)))
          (match_operand:SI 3 "register_operand" "=r"))
     (set (mem:SI (plus:SI (match_dup 1) (const_int -12)))
          (match_operand:SI 4 "register_operand" "=r"))])]
  ""
  "stm.d %1,[%2, %3, %4]")
*/

/*
(define_insn "load_multiple"
  [(set (match_operand:SI 0 "register_operand" "r")
        (match_operand:SI 1 "dop_multi_mem_operand" "m"))
   (use (match_operand:SI 2 "immediate_operand" "i"))]
  ""
  "*
    char ins[80];
    int basereg = REGNO(operands[0]);
    int numreg = INTVAL(operands[2]);
    
    sprintf(ins, \"ldm.ia %%1,@0{r%d-r%d}\", basereg, basereg+numreg-1);
    
    return ins;
  ")
*/

/*
(define_insn "store_multiple"
  [(set (match_operand:SI 0 "register_operand" "r")
        (match_operand:SI 1 "dop_multi_mem_operand" "m"))
   (use (match_operand:SI 2 "immediate_operand" "i"))]
  ""
  "*
    char ins[80];
    int basereg = REGNO(operands[0]);
    int numreg = INTVAL(operands[2]);
    
    sprintf(ins, \"stm.ia %%1,@0{r%d-r%d}\", basereg, basereg+numreg-1);
    
    return ins;
  ")
*/

(define_insn "notsi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (not:SI (match_operand:SI 1 "arith_operand" "r,I")))]
  ""
  "@
  not %0,%E1
  not %0,#%d1")

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (neg:SI (match_operand:SI 1 "arith_operand" "r")))]
  ""
  "rsb %0,%E1,#0")

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
        (plus:SI (match_operand:SI 1 "register_operand" "%r,r,r")
                 (match_operand:SI 2 "arith_or_neg_operand" "r,I,O")))]
  ""
  "@
  add %0,%E1,%E2
  add %0,%E1,#%d2
  sub %0,%E1,#%n2")

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
        (and:SI (match_operand:SI 1 "register_operand" "%r,r,r")
                (match_operand:SI 2 "arith_or_inv_operand" "r,I,P")))]
  ""
  "@
  and %0,%E1,%E2
  and %0,%E1,#%d2
  bic %0,%E1,#%v2")

(define_insn "bicsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
        (and:SI (not:SI (match_operand:SI 1 "arith_or_inv_operand" "r,I,P"))
                (match_operand:SI 2 "register_operand" "r,r,r")))]
  ""
  "@
  bic %0,%E2,%E1
  bic %0,%E2,#%d1
  and %0,%E2,#%v1")

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (ior:SI (match_operand:SI 1 "register_operand" "%r,r")
                (match_operand:SI 2 "arith_operand" "r,I")))]
  ""
  "@
  ior %0,%E1,%E2
  ior %0,%E1,#%d2")

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (xor:SI (match_operand:SI 1 "register_operand" "%r,r")
                (match_operand:SI 2 "arith_operand" "r,I")))]
  ""
  "@
  eor %0,%E1,%E2
  eor %0,%E1,#%d2")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (minus:SI (match_operand:SI 1 "register_operand" "r")
                  (match_operand:SI 2 "register_operand" "r")))]
  ""
  "@
  sub %0,%E1,%E2")

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (mult:SI (match_operand:SI 1 "register_operand" "%r,r")
                (match_operand:SI 2 "arith_operand" "r,I")))]
  ""
  "@
  mul %0,%E1,%E2
  mul %0,%E1,#%d2")

(define_expand "smulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "")
     (truncate:SI
       (lshiftrt:DI
         (mult:DI (sign_extend:DI
                    (match_operand:SI 1 "register_operand" ""))
                  (sign_extend:DI
                    (match_operand:SI 2 "arith_operand" "")))
         (const_int 32))))]
  ""
  "")


(define_insn "*smulsi3_highpart_insn"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
     (truncate:SI
       (lshiftrt:DI
         (mult:DI (sign_extend:DI
                    (match_operand:SI 1 "register_operand" "%r,r"))
                  (sign_extend:DI
                    (match_operand:SI 2 "arith_operand" "r,I")))
         (const_int 32))))]
  ""
  "@
  mlh %0,%E1,%E2
  mlh %0,%E1,#%d2")

(define_expand "umulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "")
     (truncate:SI
       (lshiftrt:DI
         (mult:DI (zero_extend:DI
                    (match_operand:SI 1 "register_operand" ""))
                  (zero_extend:DI
                    (match_operand:SI 2 "arith_operand" "")))
         (const_int 32))))]
  ""
  "")

(define_insn "*umulsi3_highpart_insn"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
     (truncate:SI
       (lshiftrt:DI
         (mult:DI (zero_extend:DI
                    (match_operand:SI 1 "register_operand" "%r,r"))
                  (zero_extend:DI
                    (match_operand:SI 2 "arith_operand" "r,I")))
         (const_int 32))))]
  ""
  "@
  umlh %0,%E1,%E2
  umlh %0,%E1,#%d2")

(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (div:SI (match_operand:SI 1 "register_operand" "r,r")
                (match_operand:SI 2 "arith_operand" "r,I")))]
  ""
  "@
  div %0,%E1,%E2
  div %0,%E1,#%d2")

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (udiv:SI (match_operand:SI 1 "register_operand" "r,r")
                 (match_operand:SI 2 "arith_operand" "r,I")))]
  ""
  "@
  udiv %0,%E1,%E2
  udiv %0,%E1,#%d2")

(define_insn "modsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (mod:SI (match_operand:SI 1 "register_operand" "r,r")
                (match_operand:SI 2 "arith_operand" "r,I")))]
  ""
  "@
  mod %0,%E1,%E2
  mod %0,%E1,#%d2")

(define_insn "umodsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (umod:SI (match_operand:SI 1 "register_operand" "r,r")
                 (match_operand:SI 2 "arith_operand" "r,I")))]
  ""
  "@
  umod %0,%E1,%E2
  umod %0,%E1,#%d2")

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (ashift:SI (match_operand:SI 1 "register_operand" "r,r")
                   (match_operand:SI 2 "arith_operand" "r,N")))]
  ""
  "@
  lsl %0,%E1,%E2
  lsl %0,%E1,#%d2")

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (ashiftrt:SI (match_operand:SI 1 "register_operand" "r,r")
                     (match_operand:SI 2 "arith_operand" "r,N")))]
  ""
  "@
  asr %0,%E1,%E2
  asr %0,%E1,#%d2")

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (lshiftrt:SI (match_operand:SI 1 "register_operand" "r,r")
                     (match_operand:SI 2 "arith_operand" "r,N")))]
  ""
  "@
  lsr %0,%E1,%E2
  lsr %0,%E1,#%d2")

(define_insn "rotrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (rotatert:SI (match_operand:SI 1 "register_operand" "r,r")
                     (match_operand:SI 2 "arith_operand" "r,N")))]
  ""
  "@
  ror %0,%E1,%E2
  ror %0,%E1,#%d2")

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=f,f")
        (neg:SF (match_operand:SF 1 "fp_op2_operand" "f,G")))]
  ""
  "@
  negf.s %0,%E1
  negf.s %0,#%G1")

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=f,f")
        (plus:SF (match_operand:SF 1 "register_operand" "%f,f")
                 (match_operand:SF 2 "fp_op2_operand" "f,G")))]
  ""
  "@
  addf.s %0,%E1,%E2
  addf.s %0,%E1,#%G2")

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=f,f")
        (minus:SF (match_operand:SF 1 "register_operand" "f,f")
                  (match_operand:SF 2 "fp_op2_operand" "f,G")))]
  ""
  "@
  subf.s %0,%E1,%E2
  subf.s %0,%E1,#%G2")

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=f,f")
        (mult:SF (match_operand:SF 1 "register_operand" "%f,f")
                 (match_operand:SF 2 "fp_op2_operand" "f,G")))]
  ""
  "@
  mulf.s %0,%E1,%E2
  mulf.s %0,%E1,#%G2")

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=f,f")
        (div:SF (match_operand:SF 1 "register_operand" "f,f")
                (match_operand:SF 2 "fp_op2_operand" "f,G")))]
  ""
  "@
  divf.s %0,%E1,%E2
  divf.s %0,%E1,#%G2")

(define_insn "negdf2"
  [(set (match_operand:DF 0 "register_operand" "=f,f")
        (neg:DF (match_operand:DF 1 "fp_op2_operand" "f,G")))]
  ""
  "@
  negf.d %0,%E1
  negf.d %0,#%G1")

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=f,f")
        (plus:DF (match_operand:DF 1 "register_operand" "%f,f")
                 (match_operand:DF 2 "fp_op2_operand" "f,G")))]
  ""
  "@
  addf.d %0,%E1,%E2
  addf.d %0,%E1,#%G2")

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=f,f")
        (minus:DF (match_operand:DF 1 "register_operand" "f,f")
                  (match_operand:DF 2 "fp_op2_operand" "f,G")))]
  ""
  "@
  subf.d %0,%E1,%E2
  subf.d %0,%E1,#%G2")

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=f,f")
        (mult:DF (match_operand:DF 1 "register_operand" "%f,f")
                 (match_operand:DF 2 "fp_op2_operand" "f,G")))]
  ""
  "@
  mulf.d %0,%E1,%E2
  mulf.d %0,%E1,#%G2")

(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=f,f")
        (div:DF (match_operand:DF 1 "register_operand" "f,f")
                (match_operand:DF 2 "fp_op2_operand" "f,G")))]
  ""
  "@
  divf.d %0,%E1,%E2
  divf.d %0,%E1,#%G2")

(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
        (sqrt:SF (match_operand:SF 1 "fp_op2_operand" "f")))]
  ""
  "sqrf.s %0,%E1")

(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
        (abs:SF (match_operand:SF 1 "fp_op2_operand" "f")))]
  ""
  "absf.s %0,%E1")

(define_insn "sqrtdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
        (sqrt:DF (match_operand:DF 1 "fp_op2_operand" "f")))]
  ""
  "sqrf.d %0,%E1")

(define_insn "absdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
        (abs:DF (match_operand:DF 1 "fp_op2_operand" "f")))]
  ""
  "absf.d %0,%E1")

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
        (float:SF (match_operand:SI 1 "register_operand" "r")))]
  ""
  "fltf.s %0,%E1")

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
        (float:DF (match_operand:SI 1 "register_operand" "r")))]
  ""
  "fltf.d %0,%E1")

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (fix:SI (match_operand:SF 1 "register_operand" "f")))]
  ""
  "fixf.s %0,%E1")

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (fix:SI (match_operand:DF 1 "register_operand" "f")))]
  ""
  "fixf.d %0,%E1")

(define_insn "extv"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extract:SI (match_operand:SI 1 "register_operand" "r")
                         (match_operand:SI 2 "immediate_operand" "i")
                         (match_operand:SI 3 "immediate_operand" "i")))]
  ""
  "*
    static char ins[80];

    int start = INTVAL(operands[3]);
    int numbits = INTVAL(operands[2]);
    
    sprintf(ins, \"bfx.s %%0,%%E1 <%d,%d>\", start, start+numbits-1);

    return ins;
  ")

(define_insn "extzv"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extract:SI (match_operand:SI 1 "register_operand" "r")
                         (match_operand:SI 2 "immediate_operand" "i")
                         (match_operand:SI 3 "immediate_operand" "i")))]
  ""
  "*
    static char ins[80];

    int start = INTVAL(operands[3]);
    int numbits = INTVAL(operands[2]);
    
    sprintf(ins, \"bfx %%0,%%E1 <%d,%d>\", start, start+numbits-1);

    return ins;
  ")

; Store operand 3 (which must be valid for word_mode) into a bit-field in
; operand 0, where operand 1 specifies the width in bits and operand 2 the
; starting bit. Operand 0 may have mode byte_mode or word_mode; often
; word_mode is allowed only for registers. Operands 1 and 2 must be valid for
; word_mode.

(define_expand "insv"
  [(set (match_dup 4)
        (unspec:SI [(match_dup 0)
                    (match_operand:SI 3 "register_operand" "r")
                    (match_dup 2)] UNSPEC_SPLS))
   (set (match_operand:SI 0 "register_operand" "=&r")
        (unspec:SI [(match_dup 4) (match_dup 0) (match_dup 5)] UNSPEC_SPL))]
  ""
{
  HOST_WIDE_INT startbit, width;
  
  if (GET_CODE (operands[1]) != CONST_INT
      || GET_CODE (operands[2]) != CONST_INT)
    abort ();

  startbit = INTVAL (operands[2]);
  width = INTVAL (operands[1]);
  
  /* If the start bit is zero, we don't need the first instruction.  */
  if (startbit == 0)
    {
      rtx insn = gen_spl (operands[0], operands[3], operands[0], operands[1]);
      emit_insn (insn);
      DONE;
    }
  
  /* If we're inserting into the top of the word, we don't need the second
     instruction.  */
  if (startbit + width >= 32)
    {
      rtx insn;

      if (startbit + width > 32)
        abort ();

      insn = gen_spls (operands[0], operands[0], operands[3], operands[2]);
      emit_insn (insn);
      DONE;
    }

  operands[4] = gen_reg_rtx (SImode);
  operands[5] = GEN_INT (startbit + width);
})

(define_insn "spl"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "register_operand" "r")
                    (match_operand:SI 2 "register_operand" "r")
                    (match_operand:SI 3 "const_int_operand" "N")] UNSPEC_SPL))]
  ""
  "spl %0,%E1,%E2,#%d3")

(define_insn "spls"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "register_operand" "r")
                    (match_operand:SI 2 "register_operand" "r")
                    (match_operand:SI 3 "const_int_operand" "N")] UNSPEC_SPLS))]
  ""
  "spl.s %0,%E1,%E2,#%d3")

(define_expand "cmpsi"
  [(set (cc0) (compare (match_operand:SI 0 "register_operand" "")
                       (match_operand:SI 1 "general_operand" "")))]
  ""
  "{
    dop_compare_op0 = operands[0];
    dop_compare_op1 = operands[1];
/*  fprintf(stderr, \"op0=%p op1=%p\n\", dop_compare_op0, dop_compare_op1);*/
    DONE;
  }")

(define_expand "cmpsf"
  [(set (cc0) (compare (match_operand:SF 0 "register_operand" "")
                       (match_operand:SF 1 "register_operand" "")))]
  ""
  "{
    dop_compare_op0 = operands[0];
    dop_compare_op1 = operands[1];
/*  fprintf(stderr, \"op0=%p op1=%p\n\", dop_compare_op0, dop_compare_op1);*/
    DONE;
  }")

(define_expand "cmpdf"
  [(set (cc0) (compare (match_operand:DF 0 "register_operand" "")
                       (match_operand:DF 1 "register_operand" "")))]
  ""
  "{
    dop_compare_op0 = operands[0];
    dop_compare_op1 = operands[1];
/*  fprintf(stderr, \"op0=%p op1=%p\n\", dop_compare_op0, dop_compare_op1);*/
    DONE;
  }")

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extend:SI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "bfx.s %0, %E1<0,7>")

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extend:SI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "bfx.s %0, %E1<0,15>")

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "bfx %0, %E1<0,7>")

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "bfx %0, %E1<0,15>")

/* movf.s is guaranteed to zero the extra double-precision bits,
 * movf.d could move garbage too (although it would seem to make more sense).
 */
(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
        (float_extend:DF (match_operand:SF 1 "register_operand" "f")))]
  ""
  "movf.s %0, %E1")

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
        (float_truncate:SF (match_operand:DF 1 "register_operand" "f")))]
  ""
  "movf.s %0, %E1")

(define_expand "seq"
  [(set (match_operand:SI 0 "register_operand" "")
        (match_dup 1))]
  ""
  "{
    operands[1] = dop_gen_setcc(EQ, dop_compare_op0, dop_compare_op1);
  }")

(define_insn "*seq_insn"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (eq:SI (match_operand:SI 1 "register_operand" "r,r")
               (match_operand:SI 2 "arith_operand" "r,I")))]
  ""
  "@
  cmp.eq %0,%E1,%E2
  cmp.eq %0,%E1,#%d2")

(define_expand "sne"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (match_dup 1))]
  ""
  "{
    operands[1] = dop_gen_setcc(NE, dop_compare_op0, dop_compare_op1);
  }")

(define_insn "*sne_insn"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (ne:SI (match_operand:SI 1 "register_operand" "r,r")
               (match_operand:SI 2 "arith_operand" "r,I")))]
  ""
  "@
  cmp.ne %0,%E1,%E2
  cmp.ne %0,%E1,#%d2")

(define_expand "slt"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (match_dup 1))]
  ""
  "{
    operands[1] = dop_gen_setcc(LT, dop_compare_op0, dop_compare_op1);
  }")

(define_insn "*slt_insn"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (lt:SI (match_operand:SI 1 "register_operand" "r,r")
               (match_operand:SI 2 "arith_operand" "r,I")))]
  ""
  "@
  cmp.lt %0,%E1,%E2
  cmp.lt %0,%E1,#%d2")

(define_expand "sgt"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (match_dup 1))]
  ""
  "{
    operands[1] = dop_gen_setcc(GT, dop_compare_op0, dop_compare_op1);
  }")

(define_insn "*sgt_insn"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (gt:SI (match_operand:SI 1 "register_operand" "r,r")
               (match_operand:SI 2 "arith_operand" "r,I")))]
  ""
  "@
  cmp.gt %0,%E1,%E2
  cmp.gt %0,%E1,#%d2")

(define_expand "sle"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (match_dup 1))]
  ""
  "{
    operands[1] = dop_gen_setcc(LE, dop_compare_op0, dop_compare_op1);
  }")

(define_insn "*sle_insn"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (le:SI (match_operand:SI 1 "register_operand" "r,r")
               (match_operand:SI 2 "arith_operand" "r,I")))]
  ""
  "@
  cmp.le %0,%E1,%E2
  cmp.le %0,%E1,#%d2")

(define_expand "sge"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (match_dup 1))]
  ""
  "{
    operands[1] = dop_gen_setcc(GE, dop_compare_op0, dop_compare_op1);
  }")

(define_insn "*sge_insn"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (ge:SI (match_operand:SI 1 "register_operand" "r,r")
               (match_operand:SI 2 "arith_operand" "r,I")))]
  ""
  "@
  cmp.ge %0,%E1,%E2
  cmp.ge %0,%E1,#%d2")

(define_expand "sltu"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (match_dup 1))]
  ""
  "{
    operands[1] = dop_gen_setcc(LTU, dop_compare_op0, dop_compare_op1);
  }")

(define_insn "*sltu_insn"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (ltu:SI (match_operand:SI 1 "register_operand" "r,r")
                (match_operand:SI 2 "arith_operand" "r,I")))]
  ""
  "@
  ucmp.lt %0,%E1,%E2
  ucmp.lt %0,%E1,#%d2")

(define_expand "sgtu"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (match_dup 1))]
  ""
  "{
    operands[1] = dop_gen_setcc(GTU, dop_compare_op0, dop_compare_op1);
  }")

(define_insn "*sgtu_insn"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (gtu:SI (match_operand:SI 1 "register_operand" "r,r")
                (match_operand:SI 2 "arith_operand" "r,I")))]
  ""
  "@
  ucmp.gt %0,%E1,%E2
  ucmp.gt %0,%E1,#%d2")

(define_expand "sleu"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (match_dup 1))]
  ""
  "{
    operands[1] = dop_gen_setcc(LEU, dop_compare_op0, dop_compare_op1);
  }")

(define_insn "*sleu_insn"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (leu:SI (match_operand:SI 1 "register_operand" "r,r")
                (match_operand:SI 2 "arith_operand" "r,I")))]
  ""
  "@
  ucmp.le %0,%E1,%E2
  ucmp.le %0,%E1,#%d2")

(define_expand "sgeu"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (match_dup 1))]
  ""
  "{
    operands[1] = dop_gen_setcc(GEU, dop_compare_op0, dop_compare_op1);
  }")

(define_insn "*sgeu_insn"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (geu:SI (match_operand:SI 1 "register_operand" "r,r")
                (match_operand:SI 2 "arith_operand" "r,I")))]
  ""
  "@
  ucmp.ge %0,%E1,%E2
  ucmp.ge %0,%E1,#%d2")

(define_insn "*seq_df_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (eq:SI (match_operand:DF 1 "register_operand" "f")
               (match_operand:DF 2 "register_operand" "f")))]
  ""
  "cmpf.d.eq %0,%E1,%E2")

(define_insn "*sne_df_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ne:SI (match_operand:DF 1 "register_operand" "f")
               (match_operand:DF 2 "register_operand" "f")))]
  ""
  "cmpf.d.ne %0,%E1,%E2")

(define_insn "*slt_df_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lt:SI (match_operand:DF 1 "register_operand" "f")
               (match_operand:DF 2 "register_operand" "f")))]
  ""
  "cmpf.d.lt %0,%E1,%E2")

(define_insn "*sgt_df_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (gt:SI (match_operand:DF 1 "register_operand" "f")
               (match_operand:DF 2 "register_operand" "f")))]
  ""
  "cmpf.d.gt %0,%E1,%E2")

(define_insn "*sle_df_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (le:SI (match_operand:DF 1 "register_operand" "f")
               (match_operand:DF 2 "register_operand" "f")))]
  ""
  "cmpf.d.le %0,%E1,%E2")

(define_insn "*sge_df_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ge:SI (match_operand:DF 1 "register_operand" "f")
               (match_operand:DF 2 "register_operand" "f")))]
  ""
  "cmpf.d.ge %0,%E1,%E2")

(define_insn "*seq_sf_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (eq:SI (match_operand:SF 1 "register_operand" "f")
               (match_operand:SF 2 "register_operand" "f")))]
  ""
  "cmpf.s.eq %0,%E1,%E2")

(define_insn "*sne_sf_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ne:SI (match_operand:SF 1 "register_operand" "f")
               (match_operand:SF 2 "register_operand" "f")))]
  ""
  "cmpf.s.ne %0,%E1,%E2")

(define_insn "*slt_sf_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lt:SI (match_operand:SF 1 "register_operand" "f")
               (match_operand:SF 2 "register_operand" "f")))]
  ""
  "cmpf.s.lt %0,%E1,%E2")

(define_insn "*sgt_sf_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (gt:SI (match_operand:SF 1 "register_operand" "f")
               (match_operand:SF 2 "register_operand" "f")))]
  ""
  "cmpf.s.gt %0,%E1,%E2")

(define_insn "*sle_sf_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (le:SI (match_operand:SF 1 "register_operand" "f")
               (match_operand:SF 2 "register_operand" "f")))]
  ""
  "cmpf.s.le %0,%E1,%E2")

(define_insn "*sge_sf_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ge:SI (match_operand:SF 1 "register_operand" "f")
               (match_operand:SF 2 "register_operand" "f")))]
  ""
  "cmpf.s.ge %0,%E1,%E2")

(define_expand "beq"
  [(set (pc)
   (if_then_else (ne (match_dup 1) (const_int 0))
                 (label_ref (match_operand 0 "dop_branch_valid_operand" ""))
                 (pc)))]
  ""
  "{
    operands[1] = dop_gen_compare_reg(EQ, dop_compare_op0, dop_compare_op1);
  }")

(define_expand "bne"
  [(set (pc)
   (if_then_else (ne (match_dup 1) (const_int 0))
                 (label_ref (match_operand 0 "dop_branch_valid_operand" ""))
                 (pc)))]
  ""
  "{
    operands[1] = dop_gen_compare_reg(NE, dop_compare_op0, dop_compare_op1);
  }")

(define_expand "blt"
  [(set (pc)
   (if_then_else (ne (match_dup 1) (const_int 0))
                 (label_ref (match_operand 0 "dop_branch_valid_operand" ""))
                 (pc)))]
  ""
  "{
    operands[1] = dop_gen_compare_reg(LT, dop_compare_op0, dop_compare_op1);
  }")

(define_expand "bgt"
  [(set (pc)
   (if_then_else (ne (match_dup 1) (const_int 0))
                 (label_ref (match_operand 0 "dop_branch_valid_operand" ""))
                 (pc)))]
  ""
  "{
    operands[1] = dop_gen_compare_reg(GT, dop_compare_op0, dop_compare_op1);
  }")

(define_expand "ble"
  [(set (pc)
   (if_then_else (ne (match_dup 1) (const_int 0))
                 (label_ref (match_operand 0 "dop_branch_valid_operand" ""))
                 (pc)))]
  ""
  "{
    operands[1] = dop_gen_compare_reg(LE, dop_compare_op0, dop_compare_op1);
  }")

(define_expand "bge"
  [(set (pc)
   (if_then_else (ne (match_dup 1) (const_int 0))
                 (label_ref (match_operand 0 "dop_branch_valid_operand" ""))
                 (pc)))]
  ""
  "{
    operands[1] = dop_gen_compare_reg(GE, dop_compare_op0, dop_compare_op1);
  }")

(define_expand "bltu"
  [(set (pc)
   (if_then_else (ne (match_dup 1) (const_int 0))
                 (label_ref (match_operand 0 "dop_branch_valid_operand" ""))
                 (pc)))]
  ""
  "{
    operands[1] = dop_gen_compare_reg(LTU, dop_compare_op0, dop_compare_op1);
  }")

(define_expand "bgtu"
  [(set (pc)
   (if_then_else (ne (match_dup 1) (const_int 0))
                 (label_ref (match_operand 0 "dop_branch_valid_operand" ""))
                 (pc)))]
  ""
  "{
    operands[1] = dop_gen_compare_reg(GTU, dop_compare_op0, dop_compare_op1);
  }")

(define_expand "bleu"
  [(set (pc)
   (if_then_else (ne (match_dup 1) (const_int 0))
                 (label_ref (match_operand 0 "dop_branch_valid_operand" ""))
                 (pc)))]
  ""
  "{
    operands[1] = dop_gen_compare_reg(LEU, dop_compare_op0, dop_compare_op1);
  }")

(define_expand "bgeu"
  [(set (pc)
   (if_then_else (ne (match_dup 1) (const_int 0))
                 (label_ref (match_operand 0 "dop_branch_valid_operand" ""))
                 (pc)))]
  ""
  "{
    operands[1] = dop_gen_compare_reg(GEU, dop_compare_op0, dop_compare_op1);
  }")

(define_insn "*dop_comparison"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (match_operator:SI 3 "comparison_operator"
                        [(match_operand:SI 1 "register_operand" "r,r")
                         (match_operand:SI 2 "arith_operand" "r,I")]))]
  ""
  "*
    const char* mnem;
    static char out[40];
  
    switch (GET_CODE(operands[3]))
    {
      case EQ: mnem = \"cmp.eq\"; break;
      case NE: mnem = \"cmp.ne\"; break;
      case LT: mnem = \"cmp.lt\"; break;
      case GT: mnem = \"cmp.gt\"; break;
      case LE: mnem = \"cmp.le\"; break;
      case GE: mnem = \"cmp.ge\"; break;
      case LTU: mnem = \"ucmp.lt\"; break;
      case GTU: mnem = \"ucmp.gt\"; break;
      case LEU: mnem = \"ucmp.le\"; break;
      case GEU: mnem = \"ucmp.ge\"; break;
      default: abort();
    }
    
    if (which_alternative==0)
      sprintf(out, \"%s %%0,%%E1,%%E2\", mnem);
    else
      sprintf(out, \"%s %%0,%%E1,#%%d2\", mnem);
      
    return out;
  ")

(define_insn "*dop_single_float_comparison"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (match_operator:SI 3 "comparison_operator"
                        [(match_operand:SF 1 "register_operand" "f")
                         (match_operand:SF 2 "register_operand" "f")]))]
  ""
  "*
    switch (GET_CODE (operands[3]))
    {
      case EQ: return \"cmpf.s.eq %0,%E1,%E2\";
      case NE: return \"cmpf.s.ne %0,%E1,%E2\";
      case LT: return \"cmpf.s.lt %0,%E1,%E2\";
      case GT: return \"cmpf.s.gt %0,%E1,%E2\";
      case LE: return \"cmpf.s.le %0,%E1,%E2\";
      case GE: return \"cmpf.s.ge %0,%E1,%E2\";
      case GEU: return \"cmpf.s.ge %0,%E1,%E2  // unsigned\";
      case GTU: return \"cmpf.s.gt %0,%E1,%E2  // unsigned\";
      case LEU: return \"cmpf.s.le %0,%E1,%E2  // unsigned\";
      case LTU: return \"cmpf.s.lt %0,%E1,%E2  // unsigned\";
      default:
      fprintf(stderr, \"Condition, %d\nOrd: %d\nLtgt: %d\",
        GET_CODE (operands[3]), ORDERED, LTGT);
      abort();
    }
  ")

(define_insn "*dop_double_float_comparison"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (match_operator:SI 3 "comparison_operator"
                        [(match_operand:DF 1 "register_operand" "f")
                         (match_operand:DF 2 "register_operand" "f")]))]
  ""
  "*
    switch (GET_CODE (operands[3]))
    {
      case EQ: return \"cmpf.d.eq %0,%E1,%E2\";
      case NE: return \"cmpf.d.ne %0,%E1,%E2\";
      case LT: return \"cmpf.d.lt %0,%E1,%E2\";
      case GT: return \"cmpf.d.gt %0,%E1,%E2\";
      case LE: return \"cmpf.d.le %0,%E1,%E2\";
      case GE: return \"cmpf.d.ge %0,%E1,%E2\";
      default:
      abort();
    }
  ")

(define_insn "*dop_conditional_branch"
  [(set (pc)
   (if_then_else (match_operator 1 "comparison_operator"
                    [(match_operand:SI 2 "register_operand" "r")
                     (const_int 0)])
                 (label_ref (match_operand 0 "dop_branch_valid_operand" ""))
                 (pc)))]
  "GET_CODE(operands[1]) == NE"
  "cbr %E2,%l0,_$cont%=\n_$cont%=:")

(define_insn "*dop_conditional_branch_inv"
  [(set (pc)
   (if_then_else (match_operator 1 "comparison_operator"
                    [(match_operand:SI 2 "register_operand" "r")
                     (const_int 0)])
                 (label_ref (match_operand 0 "dop_branch_valid_operand" ""))
                 (pc)))]
  "GET_CODE(operands[1]) == EQ"
  "cbr %E2,_$cont%=,%l0\n_$cont%=:")

(define_insn "jump"
  [(set (pc)
        (label_ref (match_operand 0 "" "")))];
  ""
  "jump %l0")

(define_insn "indirect_jump"
  [(set (pc)
        (match_operand:SI 0 "register_operand" "r"))]
  ""
  "jump %E0")

(define_insn "tablejump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))
	           (use (label_ref (match_operand 1 "" "")))]
  ""
  "jump %E0")

(define_expand "call"
  [(parallel [(call (match_operand:SI 0 "memory_operand" "")
                    (const_int 0))
              (clobber (reg:SI 54))])]
  ""
  {})

(define_insn "*call_insn"
  [(call (mem:SI (match_operand:SI 0 "dop_call_valid_operand" "ri"))
         (match_operand:SI 1 "const_int_operand" ""))
   (clobber (reg:SI 54))]
  ""
  "call %c0,_$cont%=\n_$cont%=:")

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "" "")
                   (call (match_operand:SI 1 "memory_operand" "")
                         (match_operand:SI 2 "const_int_operand" "")))
              (clobber (reg:SI 54))])]
  ""
  {})

(define_insn "*call_value_insn"
  [(set (match_operand 0 "register_operand" "")
        (call (mem:SI (match_operand:SI 1 "dop_call_valid_operand" ""))
              (match_operand 2 "const_int_operand" "")))
   (clobber (reg:SI 54))]
  ""
  "call %c1,_$cont%=\n_$cont%=:")
