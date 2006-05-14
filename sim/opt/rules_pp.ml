(* Rules representing each DOP instruction's semantics.
   (preprocessed into rules.ml by ruleparse.cmo et al during build).
*)

open Ast
open Mtree
open Asmsupport

let rules = <:insn_match<
  1: rd <- phi(args)          => phi_passthrough(rd,args);
  1: rd <- rm                 => { mov `rd, `rm };
  1: rd <- ~rm                => { not `rd, `rm };
  2: tmp(rt) := ~rm           => { not `rt, `rm };
  1: rd <- rm lsl rn          => { lsl `rd,`rm,`rn };
  2: tmp(rt) := rm lsl rn     => { lsl `rt,`rm,`rn };
  1: rd <- rm lsl s#c         => { lsl `rd,`rm,`#c };
  2: tmp(rt) := rm lsl s#c    => { lsl `rt,`rm,`#c };
  1: rd <- rm lsr rn          => { lsr `rd,`rm,`rn };
  2: tmp(rt) := rm lsr rn     => { lsr `rt,`rm,`rn };
  1: rd <- rm lsr s#c         => { lsr `rd,`rm,`#c };
  2: tmp(rt) := rm lsr s#c    => { lsr `rt,`rm,`#c };
  1: rd <- rm asr rn          => { asr `rd,`rm,`rn };
  2: tmp(rt) := rm asr rn     => { asr `rt,`rm,`rn };
  1: rd <- rm asr s#c         => { asr `rd,`rm,`#c };
  2: tmp(rt) := rm asr s#c    => { asr `rt,`rm,`#c };
  1: rd <- rm ror rn          => { ror `rd,`rm,`rn };
  2: tmp(rt) := rm ror rn     => { ror `rt,`rm,`rn };
  1: rd <- rm ror s#c         => { ror `rd,`rm,`#c };
  2: tmp(rt) := rm ror s#c    => { ror `rt,`rm,`#c };
  1: rd <- rm & rn            => { and `rd,`rm,`rn };
  2: tmp(rt) := rm & rn       => { and `rt,`rm,`rn };
  1: rd <- rm & d#c           => { and `rd,`rm,`#c };
  2: tmp(rt) := rm & d#c      => { and `rt,`rm,`#c };
  1: rd <- d#c & rm           => { and `rd,`rm,`#c };
  2: tmp(rt) := d#c & rm      => { and `rt,`rm,`#c };
  1: rd <- rm | rn            => { ior `rd,`rm,`rn };
  2: tmp(rt) := rm | rn       => { ior `rt,`rm,`rn };
  1: rd <- rm | d#c           => { ior `rd,`rm,`#c };
  2: tmp(rt) := rm | d#c      => { ior `rt,`rm,`#c };
  1: rd <- d#c | rm           => { ior `rd,`rm,`#c };
  2: tmp(rt) := d#c | rm      => { ior `rt,`rm,`#c };
  1: rd <- rm ^ rn            => { eor `rd,`rm,`rn };
  2: tmp(rt) := rm ^ rn       => { eor `rt,`rm,`rn };
  1: rd <- rm ^ d#c           => { eor `rd,`rm,`#c };
  2: tmp(rt) := rm ^ d#c      => { eor `rt,`rm,`#c };
  1: rd <- d#c ^ rm           => { eor `rd,`rm,`#c };
  2: tmp(rt) := rm ^ d#c      => { eor `rt,`rm,`#c };
  1: rd <- rm & ~rn           => { bic `rd,`rm,`rn };
  2: tmp(rt) := rm & ~rn      => { bic `rt,`rm,`rn };
  1: rd <- rm & nd#c          => { bic `rd,`rm,`#c };
  2: tmp(rt) := rm & nd#c     => { bic `rt,`rm,`#c };
  1: rd <- rm + rn            => { add `rd,`rm,`rn };
  2: tmp(rt) := rm + rn       => { add `rt,`rm,`rn };
  2: rd <- rm + md#c          => $ if match_negdataimm c then
                                     let n = negate_ast_const c in
                                     <:asm< sub `rd,`rm,`#n >>
                                   else
                                     <:asm< add `rd,`rm,`#c >> $;
  3: tmp(rt) := rm + md#c     => $ if match_negdataimm c then
                                     let n = negate_ast_const c in
                                     <:asm< sub `rt,`rm,`#n >>
                                   else
                                     <:asm< add `rt,`rm,`#c >> $;
  2: rd <- md#c + rm          => $ if match_negdataimm c then
                                     let n = negate_ast_const c in
                                     <:asm< sub `rd,`rm,`#n >>
                                   else
                                     <:asm< add `rd,`rm,`#c >> $;
  3: tmp(rt) := md#c + rm     => $ if match_negdataimm c then
                                     let n = negate_ast_const c in
                                     <:asm< sub `rt,`rm,`#n >>
                                   else
                                     <:asm< add `rt,`rm,`#c >> $;
  1: rd <- rm - rn            => { sub `rd,`rm,`rn };
  2: tmp(rt) := rm - rn       => { sub `rt,`rm,`rn };
  1: rd <- rm - d#c           => { sub `rd,`rm,`#c };
  2: tmp(rt) := rm - d#c      => { sub `rt,`rm,`#c };
  1: rd <- d#c - rm           => { rsb `rd,`rm,`#c };
  2: tmp(rt) := d#c - rm      => { rsb `rt,`rm,`#c };
  1: rd <- rm * rn            => { mul `rd,`rm,`rn };
  2: tmp(rt) := rm * rn       => { mul `rt,`rm,`rn };
  1: rd <- rm * d#c           => { mul `rd,`rm,`#c };
  2: tmp(rt) := rm * d#c      => { mul `rt,`rm,`#c };
  1: rd <- d#c * rm           => { mul `rd,`rm,`#c };
  2: tmp(rt) := d#c * rm      => { mul `rt,`rm,`#c };
  1: rd <- rm / rn            => { div `rd,`rm,`rn };
  2: tmp(rt) := rm / rn       => { div `rt,`rm,`rn };
  1: rd <- rm / d#c           => { div `rd,`rm,`#c };
  2: tmp(rt) := rm / d#c      => { div `rt,`rm,`#c };
  1: rd <- rm /u rn           => { udiv `rd,`rm,`rn };
  2: tmp(rt) := rm /u rn      => { udiv `rt,`rm,`rn };
  1: rd <- rm /u d#c          => { udiv `rd,`rm,`#c };
  2: tmp(rt) := rm /u d#c     => { udiv `rt,`rm,`#c };
  1: rd <- rm % rn            => { mod `rd,`rm,`rn };
  2: tmp(rt) := rm % rn       => { mod `rt,`rm,`rn };
  1: rd <- rm % d#c           => { mod `rd,`rm,`#c };
  2: tmp(rt) := rm % d#c      => { mod `rt,`rm,`#c };
  1: rd <- rm %u rn           => { umod `rd,`rm,`rn };
  2: tmp(rt) := rm %u rn      => { umod `rt,`rm,`rn };
  1: rd <- rm %u d#c          => { umod `rd,`rm,`#c };
  2: tmp(rt) := rm %u d#c     => { umod `rt,`rm,`#c };
  1: rd <- (re & 0x0000ffff) | eh#c => { mvc.h `rd,`#c };
  1: rd <- (re & 0xffff0000) | el#c => { mvc.l `rd,`#c };
  1: fd <- sgl fm             => { movf.s `fd,`fm };
  1: fd <- dbl fm             => { movf.d `fd,`fm };
  1: fd <- sgl f#c            => { movf.s `fd,`#c };
  1: fd <- dbl f#c            => { movf.d `fd,`#c };
  1: fd <- fm +s fn           => { addf.s `fd,`fm,`fn };
  1: fd <- fm +d fn           => { addf.d `fd,`fm,`fn };
  1: fd <- fm -s fn           => { subf.s `fd,`fm,`fn };
  1: fd <- fm -d fn           => { subf.d `fd,`fm,`fn };
  1: fd <- fm *s fn           => { mulf.s `fd,`fm,`fn };
  1: fd <- fm *d fn           => { mulf.d `fd,`fm,`fn };
  1: fd <- fm /s fn           => { divf.s `fd,`fm,`fn };
  1: fd <- fm /d fn           => { divf.d `fd,`fm,`fn };
  2: rd <- eh#c               => { mvc.eh `rd,`#c };
  3: tmp(rt) := eh#c          => { mvc.eh `rt,`#c };
  2: rd <- el#c               => { mvc.el `rd,`#c };
  3: tmp(rt) := el#c          => { mvc.el `rt,`#c };
  1: rd <- fh#c               => { mvc.fh `rd,`#c };
  3: tmp(rt) := fh#c          => { mvc.fh `rt,`#c };
  1: rd <- fl#c               => { mvc.fl `rd,`#c };
  3: tmp(rt) := fl#c          => { mvc.fl `rt,`#c };
  2: rd <- #c                 => { mvc.l `rd,`#c; mvc.h `rd,`#c };
  3: tmp(rt) := #c            => { mvc.l `rt,`#c; mvc.h `rt,`#c };
  2: rd <- word(rb)           => { ldr.w `rd,[`rb,#0] };
  3: tmp(rt) := word(rb)      => { ldr.w `rt,[`rb,#0] };
  2: rd <- word(rb + ri)      => { ldr.w `rd,[`rb,`ri] };
  3: tmp(rt) := word(rb + ri) => { ldr.w `rt,[`rb,`ri] };
  2: rd <- word(rb + wm#c)    => { ldr.w `rd,[`rb,`#c] };
  3: tmp(rt) := word(rb + wm#c) => { ldr.w `rt,[`rb,`#c] };
  2: rd <- word(wm#c + rb)    => { ldr.w `rd,[`rb,`#c] };
  3: tmp(rt) := word(wm#c + rb) => { ldr.w `rt,[`rb,`#c] };
  2: rd <- half(rb)           => { ldr.h `rd,[`rb,#0] };
  3: tmp(rt) := half(rb)      => { ldr.h `rt,[`rb,#0] };
  2: rd <- half(rb + ri)      => { ldr.h `rd,[`rb,`ri] };
  3: tmp(rt) := half(rb + ri) => { ldr.h `rt,[`rb,`ri] };
  2: rd <- half(rb + hm#c)    => { ldr.h `rd,[`rb,`#c] };
  3: tmp(rt) := half(rb + hm#c) => { ldr.h `rt,[`rb,`#c] };
  2: rd <- byte(rb)           => { ldr.b `rd,[`rb,#0] };
  3: tmp(rt) := byte(rb)      => { ldr.b `rt,[`rb,#0] };
  2: rd <- byte(rb + ri)      => { ldr.b `rd,[`rb,`ri] };
  3: tmp(rt) := byte(rb + ri) => { ldr.b `rt,[`rb,`ri] };
  2: rd <- byte(rb + bm#c)    => { ldr.b `rd,[`rb,`#c] };
  3: tmp(rt) := byte(rb + bm#c) => { ldr.b `rt,[`rb,`#c] };
  2: word(rb) <- rs         => { str.w `rs,[`rb,#0] };
  2: word(rb + ri) <- rs    => { str.w `rs,[`rb,`ri] };
  2: word(rb + wm#c) <- rs  => { str.w `rs,[`rb,`#c] };
  2: half(rb) <- rs         => { str.h `rs,[`rb,#0] };
  2: half(rb + ri) <- rs    => { str.h `rs,[`rb,`ri] };
  2: half(rb + hm#c) <- rs  => { str.h `rs,[`rb,`#c] };
  2: byte(rb) <- rs         => { str.b `rs,[`rb,#0] };
  2: byte(rb + ri) <- rs    => { str.b `rs,[`rb,`ri] };
  2: byte(rb + bm#c) <- rs  => { str.b `rs,[`rb,`#c] };
  2: sword(rb) <- sgl fs    => { stf.s `fs,[`rb,#0] };
  2: sword(rb + ri) <- sgl fs => { stf.s `fs,[`rb,`ri] };
  2: sword(rb + wm#c) <- sgl fs => { stf.s `fs,[`rb,`#c] };
  2: dword(rb) <- dbl fs    => { stf.d `fs,[`rb,#0] };
  2: dword(rb + ri) <- dbl fs => { stf.d `fs,[`rb,`ri] };
  2: dword(rb + wm#c) <- dbl fs => { stf.d `fs,[`rb,`#c] };
  2: fd <- sword(rb)        => { ldf.s `fd,[`rb,#0] };
  2: fd <- sword(rb + ri)   => { ldf.s `fd,[`rb,`ri] };
  2: fd <- sword(rb + wm#c) => { ldf.s `fd,[`rb,`#c] };
  2: fd <- dword(rb)        => { ldf.d `fd,[`rb,#0] };
  2: fd <- dword(rb + ri)   => { ldf.d `fd,[`rb,`ri] };
  2: fd <- dword(rb + wm#c) => { ldf.d `fd,[`rb,`#c] };
  1: rc <- rm == rn           => { cmp.eq `rc,`rm,`rn };
  2: tmp(rt) := rm == rn      => { cmp.eq `rt,`rm,`rn };
  1: rc <- rm == d#c          => { cmp.eq `rc,`rm,`#c };
  2: tmp(rt) := rm == d#c     => { cmp.eq `rt,`rm,`#c };
  1: rc <- rm != rn           => { cmp.ne `rc,`rm,`rn };
  2: tmp(rt) := rm != rn      => { cmp.ne `rt,`rm,`rn };
  1: rc <- rm != d#c          => { cmp.ne `rc,`rm,`#c };
  2: tmp(rt) := rm != d#c     => { cmp.ne `rt,`rm,`#c };
  1: rc <- rm <= rn           => { cmp.le `rc,`rm,`rn };
  2: tmp(rt) := rm <= rn      => { cmp.le `rt,`rm,`rn };
  1: rc <- rm <= d#c          => { cmp.le `rc,`rm,`#c };
  2: tmp(rt) := rm <= d#c     => { cmp.le `rt,`rm,`#c };
  1: rc <- rm < rn            => { cmp.lt `rc,`rm,`rn };
  2: tmp(rt) := rm < rn       => { cmp.lt `rt,`rm,`rn };
  1: rc <- rm < d#c           => { cmp.lt `rc,`rm,`#c };
  2: tmp(rt) := rm < d#c      => { cmp.lt `rt,`rm,`#c };
  1: rc <- rm >= rn           => { cmp.ge `rc,`rm,`rn };
  2: tmp(rt) := rm >= rn      => { cmp.ge `rt,`rm,`rn };
  1: rc <- rm >= d#c          => { cmp.ge `rc,`rm,`#c };
  2: tmp(rt) := rm >= d#c     => { cmp.ge `rt,`rm,`#c };
  1: rc <- rm > rn            => { cmp.gt `rc,`rm,`rn };
  2: tmp(rt) := rm > rn       => { cmp.gt `rt,`rm,`rn };
  1: rc <- rm > d#c           => { cmp.gt `rc,`rm,`#c };
  2: tmp(rt) := rm > d#c      => { cmp.gt `rt,`rm,`#c };
  1: rc <- rm <=u rn          => { ucmp.le `rc,`rm,`rn };
  2: tmp(rt) := rm <=u rn     => { ucmp.le `rt,`rm,`rn };
  1: rc <- rm <=u d#c         => { ucmp.le `rc,`rm,`#c };
  2: tmp(rt) := rm <=u d#c    => { ucmp.le `rt,`rm,`#c };
  1: rc <- rm <u rn           => { ucmp.lt `rc,`rm,`rn };
  2: tmp(rt) := rm <u rn      => { ucmp.lt `rt,`rm,`rn };
  1: rc <- rm <u d#c          => { ucmp.lt `rc,`rm,`#c };
  2: tmp(rt) := rm <u d#c     => { ucmp.lt `rt,`rm,`#c };
  1: rc <- rm >=u rn          => { ucmp.ge `rc,`rm,`rn };
  2: tmp(rt) := rm >=u rn     => { ucmp.ge `rt,`rm,`rn };
  1: rc <- rm >=u d#c         => { ucmp.ge `rc,`rm,`#c };
  2: tmp(rt) := rm >=u d#c    => { ucmp.ge `rt,`rm,`#c };
  1: rc <- rm >u rn           => { ucmp.gt `rc,`rm,`rn };
  2: tmp(rt) := rm >u rn      => { ucmp.gt `rt,`rm,`rn };
  1: rc <- rm >u d#c          => { ucmp.gt `rc,`rm,`#c };
  2: tmp(rt) := rm >u d#c     => { ucmp.gt `rt,`rm,`#c };
  1: rd <- bitf(s#a,s#b,rm)   => { bfx `rd,`rm <`na,`nb> };
  2: tmp(rt) := bitf(s#a,s#b,rm) => { bfx `rt,`rm <`na,`nb> };
  1: rd <- sbitf(s#a,s#b,rm)  => { bfx.s `rd,`rm <`na,`nb> };
  2: tmp(rt) := sbitf(s#a,s#b,rm) => { bfx.s `rt,`rm <`na,`nb> };
  2: ign trapz rc,#h          => { ldz `>h; trap.z `rc,`<h };
  2: ign trapz rc,rh          => { trap.z `rc,`rh };
  2: ign trapn rc,#h          => { ldz `>h; trap.n `rc,`<h };
  2: ign trapn rc,rh          => { trap.n `rc,`rh };
  1: ign cbr rc,#t,#f         => { ldx `>t; ldy `>f; cbr `rc,`<t,`<f };
  1: ign cbr rc,rt,#f         => { ldy `>f; cbr `rc,`rt,`<f };
  1: ign cbr rc,#t,rf         => { ldx `>t; cbr `rc,`<t,`rf };
  1: ign cbr rc,rt,rf         => { cbr `rc,`rt,`rf };
  1: ign call #t,#f           => { ldx `>t; ldy `>f; call `<t,`<f };
  1: ign call rt,#f           => { ldy `>f; call `rt,`<f };
  1: ign call #t,rf           => { ldx `>t; call `<t,`rf };
  1: ign call rt,rf           => { call `rt,`rf };
  1: ign jump #j              => { ldx `>j; jump `<j };
  1: ign jump rj              => { jump `rj };
  1: ign ret                  => { ret };
  1: ign nop                  => { nop }
>>

(*
>>
*)

(*
let Block.Block(tran,ctrl) = Translate.translist
  (<:asm< add r0,r1,r2; ret >> [])
;;

let myrule = <:rule< rd <- rm + rn >>;;
let asm =    <:asm< add `rd,`rm,`rn >>;


*)

(*
let Block.Block(hd::tl,_) = Translate.translist
  (<:asm< ldr.w r3,@0[r4,#12]; ret >> [])

let q = Select.select rules hd

let _ = Select.emit q

*)
