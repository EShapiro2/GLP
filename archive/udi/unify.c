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
**  unify.c  -  general unification
*/


#include	<stdio.h>
#include	"fcp.h"
#include	"codes.h"
#include	"global.h"
#include	"macros.h"

#include	"unify.h"


unify(V0, V1)
register heapT V0, V1;
{
  while (True) {
    register heapP P0 = Null, P1 = Null;

    deref(V0, P0);
    deref(V1, P1);
    if ((P0 != Null) && (P0 == P1)) {
      return(True);
    }
    else {
      switch (UnifyTable[Tag_of(V0)][Tag_of(V1)]) {
      case 0x00:
	/* {IntTag, RealTag, StrTag, NilTag, ListTag, TplTag, VctrTag,
	   InvldTag}, default */
	return(False);

      case 0x01:
	/* WrtTag, WrtTag */
	if (P0 > P1) {
	  asgn(V0, P0, Ref_Word(P1));
	}
	else {
	  asgn(V1, P1, Ref_Word(P0));
	}
	return(True);

      case 0x02:
	/* WrtTag, {IntTag, NilTag} */
	asgn(V0, P0, V1);
	return(True);

      case 0x03:
	/* WrtTag, {RoTag, RealTag, StrTag, ListTag, TplTag, VctrTag,
	   InvldTag} */
	asgn(V0, P0, Ref_Word(P1));
	return(True);

      case 0x04:
	/* {RoTag, RealTag, StrTag, ListTag, TplTag, VctrTag, InvldTag},
	   WrtTag */
	asgn(V1, P1, Ref_Word(P0));
	return(True);

      case 0x05:
	/* RoTag, RoTag */
	sus_tbl_add(P0);
	sus_tbl_add(P1);
	return(False);

      case 0x06:
	/* RoTag, {IntTag, RealTag, StrTag, NilTag, ListTag, TplTag, VctrTag,
	   InvldTag} */
	sus_tbl_add(P0);
	return(False);

      case 0x07:
	/* {IntTag, NilTag}, WrtTag */
	asgn(V1, P1, V0);
	return(True);

      case 0x08:
	/* {IntTag, RealTag, StrTag, NilTag, ListTag, TplTag, VctrTag,
	   InvldTag}, RoTag */
	sus_tbl_add(P1);
	return(False);

      case 0x09:
	/* IntTag, IntTag */
	return((V0 == V1));

      case 0x0a:
	/* RealTag, RealTag */
	return((real_val((P0+1)) == real_val((P1+1))));

      case 0x0b:
	/* StrTag, StrTag */
	return(str_unify(P0, P1));

      case 0x0c:
	/* NilTag, NilTag */
	return(True);

      case 0x0d:
	/* ListTag, ListTag */
	if (P0 > P1) {
	  chng(V0, P0, Ref_Word(P1));
	}
	else {
	  chng(V1, P1, Ref_Word(P0));
	}
	trls_overflow_check(AsgnTRP, ChngTRP);
	if (unify(Off_List(V0), Off_List(V1))) {
	  /* continue to unify the list */
	  V0 = Ref_Word(Cdr(P0));
	  V1 = Ref_Word(Cdr(P1));
	  continue;
	}
	return(False);

      case 0x0e:
	/* TplTag, TplTag */
	if (V0 == V1) {
	  if (P0 > P1) {
	    chng(V0, P0, Ref_Word(P1));
	  }
	  else {
	    chng(V1, P1, Ref_Word(P0));
	  }
	  {
	    register int Count = Int_Val(V0);
	    
	    for(; (Count > 0) ; Count--) {
	      trls_overflow_check(AsgnTRP, ChngTRP);
	      if (!unify(Ref_Word(++P0), Ref_Word(++P1))) {
		return(False);
	      }
	    }
	    return(True);
	  }
	}
	else {
	  return(False);
	}
      }
    }
  }
}

str_unify(Str0, Str1)
     heapP	Str0, Str1;
{
  if ((Str_Type(Str0) != Str_Type(Str1)) || 
      ((Str0 - Str_Offset(Str0)) == (Str1 - Str_Offset(Str1))) ||
      (*(Str0+1) != *(Str1+1))) {
    return(False);
  }
  else {
    register heapP	P0 = Str0+2, P1 = Str1+2;
    register int	Count = Str_Words(Str0);

    for (; Count > 0 ; Count--) {
      if (*P0++ != *P1++) {
	return(False);
      }
    }
#ifndef NCS
    if (Str_Type(Str0) != CharType) {
      return(True);
    }
    if (in_current_heap(Str0) && (Str_Offset(Str0) == 0) && 
	(!((Str0 < HB) && (Str1 >= HB)))) {
      P0 = Str0;
      P1 = Str1;
    }
    else {
      if (in_current_heap(Str1) && (Str_Offset(Str1) == 0) &&
	(!((Str1 < HB) && (Str0 >= HB)))) {
	P0 = Str1;
	P1 = Str0;
      }
      else {
	return(True);
      }
    }
    *P0++ = Ref_Word(P1);
    /* Avoid Litter */
    if (Str_Words(P1) > 1) {
      register int StrLen = Str_Length(P1) - sizeof(heapT);

      *P0++ = Str_Hdr1(CharType, 0);
      *P0 = Str_Hdr2(hash(((char *) (P0+1)), StrLen), StrLen);
    }
    else {
      *P0++ = Word(0, NilTag);
      *P0 = Word(0, NilTag);
    }
#endif
  }
  return(True);
}

/* Asgn and Chng trails have to be empty */
ask_unify(Va, Vb)
     register heapT Va;
     register heapT Vb;
{
  if (unify(Va, Vb)) {
    register trailP TRP = AsgnTRP;
    register heapP Pa, Pb = STP;

    while (TRP != AsgnTrl) {
      Pa = ((TRP--)->Address);
      if (Pa < HB) {
	*(++STP) = (heapT) Pa;
	deref_ptr(Pa);
	if ((Pa < HB) && IsVar(*Pa)) {
	  *(++STP) = (heapT) Pa;
	}
      }
    }
    if (Pb == STP) {
      asgn_trl_reset();
      /* Do not leave globals changed */
      TRP = ChngTRP;
      while (TRP != ChngTrl) {
	Pa = TRP->Address;
	if (Pa < HB) {
	  *Pa = TRP->Value;
	}
	TRP++;
      }
      chng_trl_reset();
      return(True);
    }
  }
  asgn_trl_undo(AsgnTrl);
  chng_trl_undo(ChngTrl);
  return(False);
}

/*
**  COMPARE  --  compares canonical ordering of two arguments.
**	 Ordering is:
**  integers < reals < strings < nil < lists < tuples < vectors < invalids
**  Mode conversion is done for integer-real comparisons.
**  Strings (and modules) are ordered lexically.
**  Tuples and Vectors are ordered according to arity.
**  Lists are treated as tuples and are thus all the same.
**
*/
compare(V0, V1)
     register heapT  V0, V1;
{
  while(True) {
    register heapP P0 = Null, P1 = Null;

    deref(V0, P0);
    deref(V1, P1);
    if((P0 != Null) && (P0 == P1)) {
      return(Equals); /* same term */
    }
    if (IsVar(V0)) {
      sus_tbl_add(P0);
      if (IsVar(V1)) {
	sus_tbl_add(P1);
      }
      return(False);
    }
    if (IsVar(V1)) {
      sus_tbl_add(P1);
      return(False);
    }
    switch(Tag_of(V0)) {
    case IntTag:
      switch(Tag_of(V1)) {
      case IntTag:
	if (V0 == V1) {
	  return(Equals);
	}
	return(((int) V0 < (int) V1) ? Less : Bigger);
      case RealTag:
	if (((realT) Int_Val(V0) - real_val((P1+1))) == 0.0) {
	  return(Equals);
	}
	return(((realT) Int_Val(V0) - real_val((P1+1))) < 0.0 ? Less : Bigger);
      default:
	return(Less);
      }
    case RealTag:
      switch (Tag_of(V1)) {
      case IntTag:
	if ((real_val((P0+1)) - (realT) Int_Val(V1)) == 0.0) {
	  return(Equals);
	}
	return((real_val((P0+1)) - (realT) Int_Val(V1)) < 0.0 ? Less : Bigger);
      case RealTag:
	if ((real_val((P0+1)) - real_val((P1+1))) == 0.0) {
	  return(Equals);
	}
	return((real_val((P0+1)) - real_val((P1+1))) < 0.0 ? Less : Bigger);
      default:
	return(Less);
      }
    case StrTag:
      switch(Tag_of(V1)) {
      case IntTag:
      case RealTag:
	return(Bigger);
      case StrTag:
	return(str_compare(P0, P1));
      default:
	return(Less);
      }
    case NilTag:
      switch(Tag_of(V1)) {
      case IntTag:
      case RealTag:
      case StrTag:
	return(Bigger);
      case NilTag:
	return(Equals);
      default:
	return(Less);
      }
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
      switch(Tag_of(V1)) {
      case IntTag:
      case RealTag:
      case StrTag:
      case NilTag:
	return(Bigger);
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
	chng(V0, P0, Ref_Word(P1));
	trls_overflow_check(AsgnTRP, ChngTRP);
	{
	  register int Result = compare(Off_List(V0), Off_List(V1));

	  if (Result == Equals) {
	    /* continue to examine the list */
	    V0 = Ref_Word(Cdr(P0));
	    V1 = Ref_Word(Cdr(P1));
	    continue;
	  }
	  return(Result);
	}
      default:
	return(Less);
      }
    case TplTag:
    case VctrTag:
      switch(Tag_of(V1)) {
      case IntTag:
      case RealTag:
      case StrTag:
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
	return(Bigger);
      case TplTag:
      case VctrTag:
	if (V0 == V1) {
	  register int Count = Int_Val(V0);
	  
	  chng(V0, P0, Ref_Word(P1));
	  for(; (Count > 0) ; Count--) {
	    trls_overflow_check(AsgnTRP, ChngTRP);
	    {
	      register int Result = compare(Ref_Word(++P0), Ref_Word(++P1));
	      if (Result == Equals) {
		continue;
	      }
	      return(Result);
	    }
	  }
	  return(Equals);
	}
	else {
	  if (Tag_of(V0) == Tag_of(V1)) {
	    return((V0 < V1) ? Less : Bigger);
	  }
	  else {
	    return(IsTpl(V0) ? Less : Bigger);
	  }
	}
      }
    case InvldTag:
      switch(Tag_of(V1)) {
      case InvldTag:
	if (V0 == V1) {
	  return(compare(Ref_Word(++P0), Ref_Word(++P1)));
	}
	else {
	  return((V0 < V1) ? Less : Bigger);
	}
      default:
	return(Bigger);
      }
    }
  }
}

str_compare(Str0, Str1)
     heapP Str0, Str1;
{
  if (Str_Type(Str0) != Str_Type(Str1)) {
    return((StrOrder[Str_Type(Str0)] < StrOrder[Str_Type(Str1)]) ? 
	   Less : Bigger); 
  }
  {
    register heapP P0 = Str0+2;
    register heapP P1 = Str1+2;

    {
      register int Words0 = Str_Words(Str0);
      register int Words1 = Str_Words(Str1);

      {
	register int Count = ((Words0 < Words1) ? Words0 : Words1);
    
	for (; (Count > 1) && (*P0 == *P1) ; Count--, P0++, P1++) {
	}
      }
      if (*P0 == *P1) {
	if (Words0 == Words1) {
	  return(Equals);
	}
	return((Words0 < Words1) ? Less : Bigger);
      }
    }
    {
      register char *C0 = (char *) P0;
      register char *C1 = (char *) P1;

      while (*C0 == *C1) {
	C0++; C1++;
      }
      return((*C0 < *C1) ? Less : Bigger);
    }
  }
}

int
do_grounded(V)
     register heapT V;
{
  register trailP TRPa = ChngTRP;
  register int Result = is_grounded(V);

  chng_trl_undo(TRPa);
  return(Result);
}

int
is_grounded(V)
     register heapT V;
{
  register heapP P;

  while (True) {
    deref(V, P);
    switch (Tag_of(V)) {
    case WrtTag:
    case RoTag:
      sus_tbl_add(P);
      return(False);
    case IntTag : 
    case RealTag:
    case StrTag: 
    case NilTag: 
      return(True);
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
      chng(V, P, Word(0, NilTag));
      trls_overflow_check(AsgnTRP, ChngTRP);
      if (is_grounded(Off_List(V))) {
	/* continue to check the list */
	V = Ref_Word(Cdr(P));
	continue;
      }
      return(False);
    case TplTag: 
    case VctrTag:
      chng(V, P, Word(0, NilTag));
      trls_overflow_check(AsgnTRP, ChngTRP);
      {
	register int Count = Int_Val(V);
	
	for(; (Count > 0) ; Count--) {
	  if (!is_grounded(Ref_Word(++P))) {
	    return(False);
	  }
	}
	return(True);
      }
    case InvldTag:
      return(is_grounded(Ref_Word(++P)));
    }
  }
}


