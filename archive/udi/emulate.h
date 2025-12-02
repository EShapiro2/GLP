/*
** This module is part of EFCP.
**

     Copyright 2007 Avraham Houri
     Weizmann Institute of Science, Rehovot, Israel

** EFCP is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
** 
** EFCP is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
** GNU General Public License for more details.
** 
** You should have received a copy of the GNU General Public License
** along with EFCP; if not, see:

       http://www.gnu.org/licenses

** or write to:



       Free Software Foundation, Inc.
       51 Franklin Street, Fifth Floor
       Boston, MA 02110-1301 USA

       contact: bill@wisdom.weizmann.ac.il

**
*/

/*
**  emulate.h  -  emulation time macros.
*/

/* GOTO */

#define	pr_dequeue() \
{ \
  goto dequeue_pr_label; \
}

#define	pr_fail() \
{ \
  goto fail_pr_label; \
}

#define	pr_suspend() \
{ \
  goto suspend_pr_label; \
}

#define next_instr() \
{ \
  goto next_instr_label; \
}

#define	kernel_succeeded() \
{ \
  goto kernel_succeeded_label; \
}

#define	kernel_suspended() \
{ \
  goto kernel_suspended_label; \
}

#define	kernel_failed() \
{ \
  goto kernel_failed_label; \
}

#define	emulation_start() \
{ \
  goto emulation_start_label; \
}

/* Time Slice */

#define	reset_time_slice() \
{ \
  TS = TimeSlice; \
}

#define	reduce_time_slice() \
{ \
  TS -= 1; \
}

/*
** Free Lists
*/

#define FLs_allocate(Size, P, HeapPtr) \
{ \
  if (FLs[Size] != Nil) { \
    P = FLs[Size]; \
    FLs[Size] = Ref_Val(*P); \
  } \
  else { \
    P = HeapPtr; \
    HeapPtr += Size; \
  } \
}

#define FLs_deallocate(Size, P) \
{ \
  *P = Ref_Word((FLs[Size])); \
  FLs[Size] = P; \
}

/*
**  Process records
*/

#define	pr_init(ArgsNo, P) \
{ \
  *(P) = Word(((PR_Header-1) + ArgsNo), VctrTag); \
  *(Next_PR(P)) = Ref_Word(Nil); /* next */ \
}

#define	pr_enqueue(PR, HighPriority) \
{ \
  if (QF != Nil) { \
    if (HighPriority) { \
      if (HQB == Nil) { \
	*Next_PR(PR) = Ref_Word(QF); \
	QF = HQB = PR; \
      } \
      else { \
	*Next_PR(PR) = *Next_PR(HQB); \
	*Next_PR(HQB) = Ref_Word(PR); \
        if (QB == HQB) { \
          QB = PR; \
        } \
	HQB = PR; \
      } \
    } \
    else { \
      *Next_PR(QB) = Ref_Word(PR); \
      QB = PR; \
    } \
  } \
  else { \
    QF = QB = PR; \
    if (HighPriority) { \
      HQB = QF; \
    } \
  } \
}

#define pr_activate(P) \
{ \
  if (L_Ref_Val(*P) != Nil) { \
     register heapP PR = L_Ref_Val(*P); \
     register int HighPriority = Int_Val(*Next_PR(PR)); \
\
    *Next_PR(PR) = Ref_Word(Nil); \
    pr_enqueue(PR, HighPriority); \
    debug_pr_activate(PR, HighPriority); \
    *P = L_Ref_Word(Nil); \
    Activations++; \
  } \
}

#ifdef DEBUG
#define debug_pr_activate(PR, HighPriority) \
{ \
  if (Debug_Activation) { \
    if (HighPriority) { \
      fprintf(DbgFile, "Activated (High Queue): \n"); \
    } \
    else { \
      fprintf(DbgFile, "Activated: \n"); \
    } \
    pr_print(PR); \
  } \
}
#else
#define debug_pr_activate(PR, HighPriority)
#endif

#define	pr_hangover(Suspender) \
{ \
  if (SQF != Nil) { \
    *(Next_SR(SQB)) = Ref_Word(Suspender); \
    SQB = Suspender; \
  } \
  else { \
    SQF = SQB = Suspender; \
  } \
  *(Next_PR(CP)) = Word(HighQ, IntTag); \
  CP = Nil; \
}

/*
** GC macros
*/

#define	check_gc() \
{ \
  if (ended_heap(HP)) { \
    do_gc(); \
  } \
}

/*
** Commit macros
*/

/* It is possible that in the asgn trail will be read-only variables, the
   commit operation will wake up their suspension queue */
#define do_commit1() \
{ \
  while (AsgnTRP != AsgnTrl) { \
    Va = AsgnTRP->Value; \
    if (!IsZeroed(Va)) { \
      if (IsWrt(Va)) { \
        Pa = Var_Val(Va); \
        Va = *Pa; \
        Pb = (AsgnTRP)->Address; \
        deref_ptr(Pb); \
        if (IsWrt(*Pb)) { \
	  if (IsZeroed(*Pb)) { \
	    *Pb = Var_Word(Pa, WrtTag); \
	    *Pa = Var_Word(Pb, RoTag); \
	  } \
	  else { \
	    *Pa = Ref_Word(Var_Val(*Pb)); \
	  } \
        } \
        else { \
	  if (Pa != Pb) { \
	    *Pa = Ref_Word(Pb); \
	  } \
	  else { \
	    *Pa = Var_Word(Pa, RoTag); \
	  } \
        } \
      } \
      if (IsList(*Var_Val(Va))) { \
	Pa = Var_Val(Va); \
	while (IsList(*Pa)) { \
	  Pb = L_Ref_Val(*Ref_SR(Pa)); \
	  pr_activate(Pb); \
	  Pb = Pa; \
	  Pa = Ref_Val(*Next_SR(Pa)); \
	  FLs_deallocate(SR_Size, Pb); \
	} \
      } \
    } \
    AsgnTRP--; \
  } \
}

#define do_commit0() \
{ \
  increment_reductions(); \
  debug_commit0(); \
}

#define increment_reductions() \
{ \
  Reductions++; \
  check_reductions_debug(); \
}

#ifdef	DEBUG
#define check_reductions_debug() \
{ \
  extern int ReductionsDebug; \
\
  if ((ReductionsDebug != 0) && (ReductionsDebug == Reductions)) { \
    set_debug_flags(); \
  } \
}
#else
#define check_reductions_debug()
#endif

/* The function of do_commit is to store a value into a read-only
 * variable and then wake up its suspension queue. The following steps are
 * done (only if there is a read-only variable):
 * First the addr of the read-only variable is saved in Ptra so that we can
 * do the store. Then, its "value" = start of the suspension queue is saved in
 * Value. Then we store NewVal into *Ptrb.
 *  Finally we wake up the suspension queue
*/
#define do_commit(NewValue, RoVar) \
{ \
 \
  if(!IsZeroed(RoVar)) { \
    register heapT Value; \
    register heapP Ptra, Ptrb; \
    Ptra = Var_Val(RoVar); \
 \
    Value = *Ptra; \
 \
    *Ptra = NewValue; \
 \
    if (IsList(*Var_Val(Value))) { \
          Ptra = Var_Val(Value); \
          while (IsList(*Ptra)) { \
            Ptrb = L_Ref_Val(*Ref_SR(Ptra)); \
            pr_activate(Ptrb); \
            Ptrb = Ptra; \
            Ptra = Ref_Val(*Next_SR(Ptra)); \
            FLs_deallocate(SR_Size, Ptrb); \
          } \
    } \
  } \
}


#ifdef	DEBUG
#define debug_commit0() \
{ \
  if (Debug_Process || Debug_Clause || Debug_Outargs) { \
    fprintf(DbgFile, "commited"); \
    if (Debug_Outargs) { \
      fprintf(DbgFile, ": "); \
      pr_print(CP); \
    } \
    else { \
      fprintf(DbgFile, ".\n"); \
    } \
  } \
}
#else
#define debug_commit0()
#endif

#define CG_INVOKE(f, index) \
      (*f)(W, index);

#define CG_pr_init(ArgsNo, P) \
{ \
  *(P) = Word((PR_Header + ArgsNo), VctrTag); \
  *(Next_PR(P)) = Ref_Word(Nil); /* next */ \
}

/* PC - NE */

#ifdef	ULTRIX
#define	pc_inter_offset()	((int) ((*(PC+1) << (opcodeTbits)) | *PC))
#else
#define	pc_inter_offset()	((int) ((*PC << (opcodeTbits)) | (*(PC+1))))
#endif
#define	cnv_pc_inter_offset()	{cnv_w(PC); PC += heapTbits/opcodeTbits;}

#define	pc_intra_offset()	pc_inter_offset()
#define cnv_pc_intra_offset()	cnv_pc_inter_offset()

#define pc_inter_addr()		((opcodeP) char_offset(PC, pc_inter_offset()))
#define cnv_pc_inter_addr()	cnv_pc_inter_offset()

#define pc_intra_addr()		pc_inter_addr()		
#define cnv_pc_intra_addr()	cnv_pc_intra_offset()

#define	pc_inter_offset_skip()	(intbits/charbits)

#define	pc_intra_offset_skip()  pc_inter_offset_skip()	

#define	pc_ref()	(Ref_Word(((heapP) pc_inter_addr())))
#define	cnv_pc_ref()	cnv_pc_inter_addr()

/* REG - NE */

#define reg(RegOffset)	(*((heapP) char_offset(W, RegOffset)))

#define	pc_reg()	(*((heapP) char_offset(W, *PC++)))
#define cnv_pc_reg()	cnv_s(PC++)

#define	pc_reg_offset()		(*PC++)
#define	cnv_pc_reg_offset()	cnv_s(PC++)

#define pc_reg_offset_skip()	(opcodeTbits/charbits)


/* INDEXED REG - NE */

#define	pc_subarg_addr()	((heapP) (char_offset(Ref_Val(*((heapP) \
				 char_offset(W, *PC))), \
				  ((int) *((short *) (PC+1))))))
#define	cnv_pc_subarg_addr()	{cnv_s(PC++); cnv_s(PC++);}
					      
#define	pc_subarg()		((heapT) (*pc_subarg_addr()))
#define	cnv_pc_subarg()		cnv_pc_subarg_addr()

#define pc_subarg_skip()	(2*(opcodeTbits/charbits))


/* CP - NE */

#define	pr_code_addr()	((opcodeP) (Ref_Val(*(Prcdr_PR(CP))) + CtlHdrWords))

#define	pc_cp_arg()	(*char_offset(CP, *PC++))
#define	cnv_pc_cp_arg()	cnv_s(PC++)

/* DATUM - NE */

#define pc_nibble()	(*PC++)
#define cnv_pc_nibble()	cnv_s(PC++)

#ifdef	ULTRIX
#define	pc_word()	((heapT) ((*(PC+1) << (opcodeTbits)) | *PC))
#else
#define	pc_word()	((heapT) ((*PC << (opcodeTbits)) | (*(PC+1))))
#endif
#define	cnv_pc_word()	{cnv_w(PC); PC += heapTbits/opcodeTbits;}

#define	pc_word_skip()	(heapTbits/charbits)

#define	pc_double()	real_val(PC)
#define	cnv_pc_double()	{cnv_d(PC); PC += realTbits/opcodeTbits;}

#define	pc_double_skip()	(realTbits/charbits)

#define copy_pc_double(I) \
{ \
  for (I = 0; I < (realTbits/opcodeTbits); I++) { \
    CU.C[I] = pc_nibble(); \
  } \
}
  
#define	creat_pc_double(I) \
{ \
  *HP++ = Word(0, RealTag); \
  for (I = 0; I < (realTbits/heapTbits); I++) { \
    *HP++ = pc_word(); \
    PC = (opcodeP) char_offset(PC, pc_word_skip()); \
  } \
}

#define	set_to_ro(V, P, ToP) \
{ \
  deref(V, P); \
  if (IsWrt(V)) { \
    if (IsZeroed(V)) { \
      asgn(V, P, Var_Word(ToP, WrtTag)); \
      *ToP = Var_Word(P, RoTag); \
    } \
    else { \
      *ToP = Ref_Word(Var_Val(V)); \
    } \
  } \
  else { \
    if (!(IsInt(V) || IsNil(V))) { \
      *ToP = Ref_Word(P); \
    } \
    else { \
      *ToP = V; \
    } \
  } \
}

#define	set_to_ro_ref(V, P) \
{ \
  deref(V, P); \
  if (IsWrt(V)) { \
    if (IsZeroed(V)) { \
      asgn(V, P, Var_Word(HP, WrtTag)); \
      V = Ref_Word(HP); \
      *HP++ = Var_Word(P, RoTag); \
    } \
    else { \
      V = Ref_Word(Var_Val(V)); \
    } \
  } \
  else { \
    if (!(IsInt(V) || IsNil(V))) { \
      V = Ref_Word(P); \
    } \
  } \
}

#define	set_to_car(V) \
{ \
  deref_val(V); \
  V = Off_List(V); \
}

#define	load_pr_args(I, P) \
{ \
  /* Load Process Arguments */ \
  I = ArgsNo_PR(CP); \
  P = Args_PR(CP) + I - 1; \
  for (; I > 0; I--) { \
    X[I] = *P--; \
  } \
}

#define	store_pr_args(I, P) \
{ \
  /* Store Process Arguments */ \
  I = ArgsNo_PR(CP); \
  P = Args_PR(CP) + I - 1; \
  for (; I > 0; I--) { \
    *P-- = X[I]; \
  } \
}

#define end_of_branch() \
{ \
  HP = HB; \
  asgn_trl_undo(AsgnTrl); \
  chng_trl_undo(ChngTrl); \
  if (!err_tbl_empty()) { \
    if (check_pr_retry()) { \
      load_pr_args(Ia, Pa); \
      PC = pr_code_addr(); \
      next_instr(); \
    } \
  } \
  debug_end_of_branch(); \
}

#ifdef DEBUG
#define debug_end_of_branch() \
{ \
  if (Debug_Clause) { \
    fprintf(DbgFile, "end of branch. sus table is %sempty.\n", \
	    (sus_tbl_empty() ? "" : "not ")); \
  } \
}
#else
#define debug_end_of_branch()
#endif

/* Kernels debuging */

#ifdef	DEBUG

#define	debug_ask_kernel_0(Name) \
  print_kernel_0("Ask", Name)

#define	debug_ask_kernel_1(Name, Arg1) \
  print_kernel_1("Ask", Name, Arg1)

#define	debug_ask_kernel_2(Name, Arg1, Arg2) \
  print_kernel_2("Ask", Name, Arg1, Arg2)

#define	debug_ask_kernel_3(Name, Arg1, Arg2, Arg3) \
  print_kernel_3("Ask", Name, Arg1, Arg2, Arg3)

#define	debug_ask_kernel_4(Name, Arg1, Arg2, Arg3, Arg4) \
  print_kernel_4("Ask", Name, Arg1, Arg2, Arg3, Arg4)

#define	debug_ask_kernel_5(Name, Arg1, Arg2, Arg3, Arg4, Arg5) \
  print_kernel_5("Ask", Name, Arg1, Arg2, Arg3, Arg4, Arg5)

#define	debug_tell_kernel_0(Name) \
  print_kernel_0("Tell", Name)

#define	debug_tell_kernel_1(Name, Arg1) \
  print_kernel_1("Tell", Name, Arg1)

#define	debug_tell_kernel_2(Name, Arg1, Arg2) \
  print_kernel_2("Tell", Name, Arg1, Arg2)

#define	debug_tell_kernel_3(Name, Arg1, Arg2, Arg3) \
  print_kernel_3("Tell", Name, Arg1, Arg2, Arg3)

#define	debug_tell_kernel_4(Name, Arg1, Arg2, Arg3, Arg4) \
  print_kernel_4("Tell", Name, Arg1, Arg2, Arg3, Arg4)

#else

#define	debug_ask_kernel_0(Name)

#define	debug_ask_kernel_1(Name, Arg1)

#define	debug_ask_kernel_2(Name, Arg1, Arg2)

#define	debug_ask_kernel_3(Name, Arg1, Arg2, Arg3)

#define	debug_ask_kernel_4(Name, Arg1, Arg2, Arg3, Arg4)

#define	debug_ask_kernel_5(Name, Arg1, Arg2, Arg3, Arg4, Arg5)

#define	debug_tell_kernel_0(Name)

#define	debug_tell_kernel_1(Name, Arg1)

#define	debug_tell_kernel_2(Name, Arg1, Arg2)

#define	debug_tell_kernel_3(Name, Arg1, Arg2, Arg3)

#define	debug_tell_kernel_4(Name, Arg1, Arg2, Arg3, Arg4)

#define	debug_tell_kernel_5(Name, Arg1, Arg2, Arg3, Arg4, Arg5)

#endif

