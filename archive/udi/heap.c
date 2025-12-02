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


** 
**     Free Software Foundation, Inc.
**     51 Franklin Street, Fifth Floor
**     Boston, MA 02110-1301 USA

       contact: bill@wisdom.weizmann.ac.il
*/

/*
**	heap.c  -  kernel predicates dealing with the entire heap
**
**	These routines implement the kernel guard predicates
**	which are executed through the call assembler instruction.
**
**	Called By:	emulate()  via  kernel_fns function table
**
**	21 Sivan 5745, 10 Jun 1985
**
*/

#include	<stdio.h>
extern	FILE *DbgFile;
#include	<errno.h>

#include	"fcp.h"
#include	"codes.h"
#include	"global.h"
#include	"macros.h"
#include	<sys/file.h>
#include	<sys/time.h>
#include	<time.h>
#include	<sys/resource.h>

heapP LostProcesses;

do_gc()
{
  int OldHeapSpace = CurHeapEnd - HP;
  heapP Base = OtherHeap;
  register heapP LQF = Nil, LQB = Nil;
  register unsigned int StartCpuTime;

  struct rusage	R_UsageStart, R_UsageEnd;

  /* Count garbage collection time, Seperately */ 
  stop_time();
  StartCpuTime = CpuTime;
  start_time();

  if (getrusage(RUSAGE_SELF, &R_UsageStart) < 0) {
    do_exit("getrusage - RUSAGE_SELF", SYSTEM, errno, False);
  }

  if (HP >= CurHeapEnd) {
    do_exit("garbage collection - start", MACHINE, ErHPOVFL, True);
  }

  fprintf(DbgFile, "(GC)");
  fflush(DbgFile);
  HP = Base;
  /* Copy Active Queue */
  if (QF != Nil) {
    register heapP P0 = QF;

    while (P0 != Nil) {
      register heapP P1 = P0, P2 = P0 + 1 + Val_of(*P0), P3 = HP;

      while (P1 != P2) {
	*HP++ = *P1++;
      }
      *P0 = Ref_Word(P3);
      P0 = Ref_Val(*Next_PR(P3));
    }
    QF = Ref_Val(*QF);
    if (HQB != Nil) {
      HQB = Ref_Val(*HQB);
    }
    QB = Ref_Val(*QB);
  }
  /* Prepare Suspended Queue */
  if (SQF != Nil) {
    register heapP P0 = SQF;

    while ((P0 != Nil) && (L_Ref_Val(*Ref_SR(P0)) == Nil)) {
      P0 = Ref_Val(*Next_SR(P0));
    }
    if (P0 != Nil) {
      register heapP P1 = P0;

      SQF = P0;
      P0 = Ref_Val(*Next_SR(P0));
      while (P0 != Nil) {
	if (L_Ref_Val(*Ref_SR(P0)) != Nil) {
	  *Next_SR(P1) = Ref_Word(P0);
	  P1 = P0;
	}
	P0 = Ref_Val(*Next_SR(P0));
      }
      *Next_SR(P1) = Ref_Word(Nil);
      SQB = P1;
    }
    else {
      SQF = SQB = Nil;
    }
  }
  nil_FLs();
  if (CP != Nil) {
    *HP++ = Ref_Word(CP);
  }
  /* Prepare Streams */
  deref_ptr(McnInP);
  *HP++ = Ref_Word(McnInP);
  deref_ptr(McnOutP);
  *HP++ = Ref_Word(McnOutP);

  prepare_devices_streams_for_gc();

  collect(Base);
  Base = HP;
  /* Form Suspended Queue and Add to Lost Queue */
  if (SQF != Nil) {
    register heapP P0 = SQF;

    SQF = SQB = Nil;
    while (P0 != Nil) {
      if (IsRef(*P0)) { /* Suspender was Copied */
	if (SQF != Nil) {
	  *(Next_SR(SQB)) = *P0;
	  SQB = Ref_Val(*P0);
	}
	else {
	  SQF = SQB = Ref_Val(*P0);
	}
      }
      else {
	*HP = *P0;
	/* Nil the old hanger in order to prevent activations while dealing
	   with the lost precesses */
	*P0 = L_Ref_Word(Nil);
	if (LQF != Nil) {
	  *(Next_SR(LQB)) = Ref_Word(HP);
	  LQB = HP;
	}
	else {
	  LQF = LQB = HP;
	}
	HP += 2;
	Losses++;
      }
      P0 = Ref_Val(*Next_SR(P0));
    }
    if (LQB != Nil) {
      *(Next_SR(LQB)) = Ref_Word(Nil);
    }
  }
  if (Base != HP) {
    collect(Base);
  }
  if (CP != Nil) {
    CP = Ref_Val(*CP);
  }
  /* Adjust Streams */
  McnInP = Ref_Val(*McnInP);
  McnOutP = Ref_Val(*McnOutP);

  adjust_devices_streams_after_gc();

  HB = HP;
  /* Adjust Heaps */
  {
    register int Difference = 
      abs((OtherHeapEnd - OtherHeap) - (CurHeapEnd - CurHeap))/2;

    if (Difference > 0) {
      if (CurHeap > OtherHeap) {
	OtherHeapEnd = CurHeap += Difference;
      }
      else {
	CurHeapEnd = OtherHeap += Difference;
      }
    }
  }
  /* Switch Heaps */
  {
    register heapP P;

    P = CurHeap;
    CurHeap = OtherHeap;
    OtherHeap = P;
    P = CurHeapEnd;
    CurHeapEnd = OtherHeapEnd;
    OtherHeapEnd = P;
    CurHeapLimit = CurHeapEnd - Heap_TH;
  }
  if (heap_space(sizeof(heapT)) < Heap_TH) {
    do_exit("garbage collection - end", MACHINE, ErHPOVFL, True);
  }
  Collections++;
#ifdef	DEBUG
  {
    extern int GCDebug;

    if ((GCDebug != 0) && (GCDebug == Collections)) {
      Debug = True;
    }
  }
  if (Debug_Buffered) {
    /* reset ./debug.logix */
    DbgFile = fopen("debug.logix", "w");
    if (DbgFile == NULL) {
      DbgFile = stdout;
      fprintf(DbgFile, "Can not reopen 'debug.logix'\n");
      Debug_Buffered = False;
    }
  }
#endif

  LostProcesses = LQF;

  mcn_output(GCC);
  fprintf(DbgFile, "    ");
  fflush(DbgFile);

  /* Statistics */
  FreedAverage += ((((CurHeapEnd-HP)-OldHeapSpace)-((int) FreedAverage))/
		   ((int) Collections));
  CopiedAverage += (((HP-CurHeap)-((int) CopiedAverage))/((int) Collections));

  if (getrusage(RUSAGE_SELF, &R_UsageEnd) < 0) {
    do_exit("getrusage - RUSAGE_SELF", SYSTEM, errno, False);
  }
  /* GC page faults */
  GCMinFlt += R_UsageEnd.ru_minflt - R_UsageStart.ru_minflt;
  GCMajFlt += R_UsageEnd.ru_majflt - R_UsageStart.ru_majflt;

  /* Count garbage collection time, Seperately */ 
  stop_time();
  GCTime += CpuTime - StartCpuTime;
  start_time();
}

nil_FLs()
{
  register heapP *P = FLs, *PEnd = FLs + FLsSize;
  
  for (; P < PEnd; P++) {
    *P = Nil;
  }
}

collect(Base)
     heapP Base;
{
  register heapP C_HP = Base;
  register heapP NotAddress = Nil;

  while (C_HP < HP) {
    switch (Tag_of(*C_HP)) {
    case Tag(0x00, RefFlag):
    case Tag(0x01, RefFlag):
    case Tag(0x02, RefFlag):
    case Tag(0x03, RefFlag):
    case Tag(0x04, RefFlag):
    case Tag(0x05, RefFlag):
    case Tag(0x06, RefFlag):
    case Tag(0x07, RefFlag):
    case Tag(0x08, RefFlag):
    case Tag(0x09, RefFlag):
    case Tag(0x0a, RefFlag):
    case Tag(0x0b, RefFlag):
    case Tag(0x0c, RefFlag):
    case Tag(0x0d, RefFlag):
    case Tag(0x0e, RefFlag):
    case Tag(0x0f, RefFlag):
      if (in_current_heap(Ref_Val(*C_HP))) {
	break;
      }
      C_HP++;
      continue;
    case WrtTag:
      if (!IsZeroed(*C_HP) && in_current_heap(Var_Val(*C_HP))) {
	register heapP P = Var_Val(*C_HP);
	
	if (IsRef(*P)) {
	  *C_HP = Var_Word(Ref_Val(*P), Tag_of(*C_HP));
	}
	else {
	  *C_HP = Var_Word(HP, Tag_of(*C_HP));
	  *HP++ = *P;
	  *P = Ref_Word((HP-1));
	}
      }
      C_HP++;
      continue;
    case RoTag:
      if (in_current_heap(Var_Val(*C_HP))) {
	register heapP P0 = Var_Val(*C_HP);
	register heapP P1 = HP;
	register heapP P2 = HP;
	
	while (IsList(*P0)) {
	  if (IsRef(*(L_Ref_Val(*Ref_SR(P0))))) {
	    if (HP != P1) {
	      *Next_SR(P1) = Ref_Word(HP);
	      P1 = HP;
	    }
	    *HP++ = Set_List(*(L_Ref_Val(*Ref_SR(P0))));
	    HP++;
	  }
	  else {
	    if ((L_Ref_Val(*(L_Ref_Val(*Ref_SR(P0)))) != Nil) ||
		(IsVctr(*Ref_Val(*Next_SR(L_Ref_Val(*Ref_SR(P0))))))) {
	      if (HP != P1) {
		*Next_SR(P1) = Ref_Word(HP);
		P1 = HP;
	      }
	      *HP = L_Ref_Word((HP+2));
	      HP+=2;
	      {
		register heapP P3 = L_Ref_Val(*Ref_SR(P0));
		  
		*HP++ = *P3;
		if (IsVctr(*Ref_Val(*Next_SR(P3)))) {
		  *HP++ = *Next_SR(P3);
		}
		else {
		  *HP++ = Ref_Word(Nil);
		}
		*P3 = Ref_Word((HP-2));
	      }
	    }
	  }
	  P0 = Ref_Val(*Next_SR(P0));
	}
	if (in_current_heap(P0)) {
	  if (IsRef(*P0)) {
	    P0 = Ref_Val(*P0);
	  }
	  else {
	    *HP = *P0;
	    *P0 = Ref_Word(HP);
	    if (HP == P2) {
	      P2++;
	    }
	    P0 = HP++;
	  }	    
	}
	if (HP != P2) {
	  *Next_SR(P1) = Ref_Word(P0);
	  *C_HP = Var_Word(P2, RoTag);
	}
	else {
	  *C_HP = Var_Word(P0, RoTag);
	}
      }
      C_HP++;
      continue;
    case IntTag:
      C_HP++;
      continue;
    case RealTag:
      C_HP++;
      C_HP += realTbits/heapTbits;
      continue;
    case StrTag:
      C_HP += 2 + Str_Words(C_HP);  /* skip over item */
      continue;
    case NilTag :
      C_HP++;
      continue;
    case Tag(0x00, L_RefFlag):
    case Tag(0x01, L_RefFlag):
    case Tag(0x02, L_RefFlag):
    case Tag(0x03, L_RefFlag):
    case Tag(0x04, L_RefFlag):
    case Tag(0x05, L_RefFlag):
    case Tag(0x06, L_RefFlag):
    case Tag(0x07, L_RefFlag):
    case Tag(0x08, L_RefFlag):
    case Tag(0x09, L_RefFlag):
    case Tag(0x0a, L_RefFlag):
    case Tag(0x0b, L_RefFlag):
    case Tag(0x0c, L_RefFlag):
    case Tag(0x0d, L_RefFlag):
    case Tag(0x0e, L_RefFlag):
    case Tag(0x0f, L_RefFlag):
      if in_current_heap(L_Ref_Val(*C_HP)) {
	NotAddress = C_HP;
	break;
      }
      *C_HP++;
      continue;
    case L_IntTag: 
    case L_NilTag: 
    case TplTag:
      C_HP++;
      continue;
    case VctrTag:
      NotAddress = C_HP + Arity_of(*C_HP);
      C_HP++;
      continue;
    case InvldTag:
      C_HP++;
      continue;
    default:
      do_exit("garbage collection - collect:1", MACHINE, ErINVLDOBJ, False);
    }
    {
      register heapP P = C_HP;

      while (P != Nil) {
	register heapP P0 = P;
	register heapT V0 = Off_List(*P0);

	*P &= ListFlag;
	deref(V0, P0);
	if (in_current_heap(P0)) {
	  switch (Tag_of(V0)) {
	  case WrtTag:
	  case RoTag:
	    if (P > NotAddress) {
	      *P = V0;
	      *P0 = Ref_Word(P);
	    }
	    else {
	      *P |= Ref_Word(HP);
	      *HP++ = V0;
	      *P0 = Ref_Word((HP-1));
	    }
	    P = Nil;
	    continue;
	  case IntTag:
	    *P |= V0;
	    P = Nil;
	    continue;
	  case RealTag:
	    if ((P > NotAddress) && ((P+1) == HP)) {
	      HP--;
	    }
	    else {
	      *P |= Ref_Word(HP);
	    }
	    *HP = V0;
	    *P0++ = Ref_Word(HP++);
	    real_copy(P0, HP);;
	    HP += realTbits/heapTbits;
	    P = Nil;
	    continue;
	  case StrTag:
	    {
	      register int Offset = Str_Offset(P0);
	      register heapP CopiedStr = P0 - Offset;

	      if (IsRef(*CopiedStr)) { /* Already Copied */
		*P |= *P0 = Ref_Word((Ref_Val(*CopiedStr)+Offset));
	      }
	      else {
		if ((Offset == 0) && (P > NotAddress) && ((P+1) == HP)) {
		  HP--;
		}
		else {
		  *P |= Ref_Word((HP+Offset));
		}
		{
		  register int	Count = 2 + Str_Words(CopiedStr);

		  P = HP;
		  for (; Count > 0; Count--) {
		    *HP++ = *CopiedStr++;
		  }
		  if (Offset > 0) {
		    *(P0 - Offset) = Ref_Word(P);
		  }
		  *P0 = Ref_Word((P+Offset));
		}
	      }
	    }
	    P = Nil;
	    continue;
	  case NilTag:
	    *P |= V0;
	    P = Nil;
	    continue;
	  case Tag(0x00, L_RefFlag):
	  case Tag(0x01, L_RefFlag):
	  case Tag(0x02, L_RefFlag):
	  case Tag(0x03, L_RefFlag):
	  case Tag(0x04, L_RefFlag):
	  case Tag(0x05, L_RefFlag):
	  case Tag(0x06, L_RefFlag):
	  case Tag(0x07, L_RefFlag):
	  case Tag(0x08, L_RefFlag):
	  case Tag(0x09, L_RefFlag):
	  case Tag(0x0a, L_RefFlag):
	  case Tag(0x0b, L_RefFlag):
	  case Tag(0x0c, L_RefFlag):
	  case Tag(0x0d, L_RefFlag):
	  case Tag(0x0e, L_RefFlag):
	  case Tag(0x0f, L_RefFlag):
	  case L_IntTag:
	  case L_NilTag:
	    if ((P > NotAddress) && ((P+1) == HP)) {
	      HP--;
	    }
	    else {
	      *P |= Ref_Word(HP);
	    }
	    *HP++ = V0;
	    *P0 = Ref_Word((HP-1));
	    P = HP;
	    *HP++ = Ref_Word(Cdr(P0));
	    continue;
	  case TplTag:
	    if ((P > NotAddress) && ((P+1) == HP)) {
	      HP--;
	    }
	    else {
	      *P |= Ref_Word(HP);
	    }
	    {
	      register int Count = Val_of(V0) - 1;

	      *HP++ = V0;
	      *P0++ = Ref_Word((HP-1));
	      for (; Count > 0; Count--) {
		if (IsWrt(*P0) || IsRo(*P0)) {
		  *HP++ = *P0;
		  *P0++ = Ref_Word((HP-1));
		}
		else {
		  *HP++ = *P0++;
		}
	      }
	    }
	    P = HP;
	    *HP++ = Ref_Word(P0);
	    continue;
	  case VctrTag:
	    *P |= Ref_Word(HP);
	    {
	      register int Count = Arity_of(V0);

	      *HP++ = V0;
	      *P0++ = Ref_Word((HP-1));
	      for (; Count > 0; Count--) {
		*HP++ = *P0++;
	      }
	    }
	    P = Nil;
	    continue;
	  case InvldTag:
	    if ((P > NotAddress) && ((P+1) == HP)) {
	      HP--;
	    }
	    else {
	      *P |= Ref_Word(HP);
	    }
	    *HP++ = V0;
	    *P0++ = Ref_Word((HP-1));
	    *HP++ = *P0++;
	    P = Nil;
	    continue;
	  default:
	    do_exit("garbage collection - collect:2", MACHINE, ErINVLDOBJ,
		    False);
	  }
	}
	else {
	  *P |= Ref_Word(P0);
	  P = Nil;
	}
      }
    }
  }
}

/* Freeze */

freezeheap()    /* prevent module movement during garbage collection */
{               
  register heapP C_HP;

  do_gc();
  if (CurHeap < OtherHeap) {     /* force everything to the upper heap */
    do_gc();
  }
  C_HP = CurHeap;
  while (C_HP < HP) {
    switch (Tag_of(*C_HP)) {
    case Tag(0x00, RefFlag):
    case Tag(0x01, RefFlag):
    case Tag(0x02, RefFlag):
    case Tag(0x03, RefFlag):
    case Tag(0x04, RefFlag):
    case Tag(0x05, RefFlag):
    case Tag(0x06, RefFlag):
    case Tag(0x07, RefFlag):
    case Tag(0x08, RefFlag):
    case Tag(0x09, RefFlag):
    case Tag(0x0a, RefFlag):
    case Tag(0x0b, RefFlag):
    case Tag(0x0c, RefFlag):
    case Tag(0x0d, RefFlag):
    case Tag(0x0e, RefFlag):
    case Tag(0x0f, RefFlag):
    case WrtTag:
    case RoTag:
    case IntTag:
      C_HP++;
      continue;
    case RealTag:
      C_HP++;
      C_HP += realTbits/heapTbits;
      continue;
    case StrTag:
      if (Str_Type(C_HP) == MdlType) {
	register heapP P0 = OtherHeap;

	OtherHeap += 2 + Str_Words(C_HP);
	*P0 = *C_HP;
	*C_HP++ = Ref_Word(P0);
	P0++;
	while (P0 < OtherHeap) {
	  *P0++ = *C_HP++;
	}
      }
      else {
	C_HP += 2 + Str_Words(C_HP);
      }
      continue;
    case NilTag:
    case Tag(0x00, L_RefFlag):
    case Tag(0x01, L_RefFlag):
    case Tag(0x02, L_RefFlag):
    case Tag(0x03, L_RefFlag):
    case Tag(0x04, L_RefFlag):
    case Tag(0x05, L_RefFlag):
    case Tag(0x06, L_RefFlag):
    case Tag(0x07, L_RefFlag):
    case Tag(0x08, L_RefFlag):
    case Tag(0x09, L_RefFlag):
    case Tag(0x0a, L_RefFlag):
    case Tag(0x0b, L_RefFlag):
    case Tag(0x0c, L_RefFlag):
    case Tag(0x0d, L_RefFlag):
    case Tag(0x0e, L_RefFlag):
    case Tag(0x0f, L_RefFlag):
    case L_IntTag:
    case L_NilTag:
      C_HP++;
      continue;
    case TplTag:
      C_HP += Val_of(*C_HP);	/* skip to last arg */
      continue;
    case VctrTag:
      C_HP += 1 + Val_of(*C_HP);
      continue;
    case InvldTag:
      C_HP++;
      C_HP++;
      continue;
    default: 
      do_exit("freeze", MACHINE, ErINVLDOBJ, False);
    }
  }
  do_gc(); /* Move Remainder */
}

/* Melt */

meltheap()  /* allow modules to be moved during garbage collection */
{
  if (CurHeap > OtherHeap) {  /* force into lower heap */
    do_gc();
  }
  if ((HeapStart + (HeapEnd - HeapStart)/2) > HP) {
    CurHeap = HeapStart;
    CurHeapEnd = HeapStart + (HeapEnd - HeapStart)/2;
    CurHeapLimit = CurHeapEnd - Heap_TH;
    OtherHeap = CurHeapEnd;
    OtherHeapEnd = HeapEnd;
    return(True);
  }
  else {
    return(False);
  }
}

/* Save procedures */

save_vars()
{
  heapP	P = HP;

  *HP++ = (heapT) LP;

  *HP = (heapT) HP;
  HP++;
  *HP++ = (heapT) HB;

  *HP++ = (heapT) QF;
  *HP++ = (heapT) HQB;
  *HP++ = (heapT) QB;

  *HP++ = (heapT) SQF;
  *HP++ = (heapT) SQB;

  HP += memcopy((heapP) FLs, HP, (FLsSize)*sizeof(heapP));

  *HP++ = (heapT) CP;

  *HP++ = (heapT) TimeSlice;
  *HP++ = (heapT) DevsTime;

  *HP++ = (heapT) McnInP;
  *HP++ = (heapT) McnOutP;
  *HP++ = (heapT) McnOutM;

  *HP++ = (heapT) Nil;
  *HP++ = (heapT) SVRMarker;
  HP += memcopy((heapP) Constants, HP, (ConstSize)*sizeof(heapP));

  *HP++ = (heapT) (Creations - Terminations);		/* Creations */

							/* Suspensions */
  *HP++ = (heapT) (Suspensions - (Activations + Failures + Losses));

  *HP++ = (heapT) P;
}

rstr_vars()
{
  extern int SpecTime;
  extern int SpecDevsTime;

  HP--;
  HP = (heapP) *HP;

  LP = (linkP) *HP++;

  HP = (heapP) *HP;
  HP++;
  HB = (heapP) *HP++;

  QF = (heapP) *HP++;
  HQB = (heapP) *HP++;
  QB = (heapP) *HP++;

  SQF = (heapP) *HP++;
  SQB = (heapP) *HP++;

  HP += memcopy(HP, (heapP) FLs, (FLsSize)*sizeof(heapP));

  CP = (heapP) *HP++;

  if (SpecTime) {
    HP++;
  }
  else {
    TimeSlice = (int) *HP++;
  }
  if (SpecDevsTime) {
    HP++;
  }
  else {
    DevsTime = (int) *HP++;
  }

  McnInP = (heapP) *HP++;
  McnOutP = (heapP) *HP++;
  McnOutM = (int) *HP++;

  Nil = (heapP) *HP++;
  SVRMarker = (heapP) *HP++;
  HP += memcopy(HP, (heapP) Constants, (ConstSize)*sizeof(heapP));

  Creations = (int) *HP++;
  Suspensions = (int) *HP++;

  HP = (heapP) *HP;
}

int
memcopy(PFrom, PTo, Len)
     heapP	PFrom;
     heapP	PTo;
     int	Len;
{
  register heapP P1 = PFrom, P2 = PTo;
  register int	 Count;

  for (Count = Len ; Count > 0 ; Count -= sizeof(heapT)) {
    *P2++ = *P1++;
  }
  return(P1 - PFrom);
}
