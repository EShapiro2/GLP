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
 **	fcp.h  -  defines data structures, machine word format
 **		  and various macros
 **
 **	NOTE when changing macros, to leave things as single occurence.
 **
 */

#define False	 0
#define	True	 1

#define	Less	 2
#define Equals	 3
#define Bigger	 4

#define	EmulationBoot	0
#define	NativeBoot	1
#define	WarmBoot	2

/* Basic typedefs */

typedef	char	linkT;		/* link "cell" */
typedef	linkT	*linkP;		/* pointer to link "cell" */

typedef	unsigned int	heapT;		/* heap word */
typedef	heapT		*heapP;		/* pointer to heap word */

typedef	unsigned short	opcodeT;	/* code word */
typedef opcodeT		*opcodeP;	/* pointer to code word */

typedef   double   realT;
typedef   realT    *realP;

/* Basic Sizes */

#define charbits	8
#define	shortbits	(sizeof(short)*charbits)
#define	intbits		(sizeof(int)*charbits)
#define	unsignedbits	(sizeof(unsigned)*charbits)
#define linkTbits	(sizeof(linkT)*charbits)
#define heapTbits	(sizeof(heapT)*charbits)
#define opcodeTbits	(sizeof(opcodeT)*charbits)
#define opcodePbits	(sizeof(opcodeP)*charbits)
#define	realTbits	(sizeof(realT)*charbits)

typedef union {
  int	I;
  heapT H;
  double D;
  opcodeT C[realTbits/opcodeTbits];
} codeU;

typedef struct { /* Trail entry */
  heapT	Value;
  heapP	Address;
} trailT;

typedef trailT	*trailP;

/* Data structures typedefs */

/*
 ** Reserved
 */

#define	Def_RsrvSize	0

/*
 ** Link
 */

#define	Def_LinkSize	5000 * 1024

#define	ended_link(P)	((linkP) P >= LinkEnd)
  
/*
 ** Heap
 */
  
#define	Def_HeapSize	5000*1024*2*sizeof(heapT)
#define Heap_TH		200*1024

#define Null	((heapP) 0)
  
#define ended_heap(P)  ( ((heapP) (P) < CurHeapLimit) ? False : heap_ended((heapP) (P)) )
#define	heap_space(UnitSize)	(((CurHeapLimit - HP)*sizeof(heapT))/UnitSize)
#define in_current_heap(P)	((P >= CurHeap) && (P < CurHeapEnd))

/*
 ** Code
 */

#define MaxopcodeT	0xffff
#define opcodeSignBit	0x8000
#define opcodeSignExt	0xffff0000
#define MaxPrcdrArgs	0xfc

/*
** Trails
*/

#define Def_TrlsSize	60000*sizeof(trailT)
#define Trls_TH		20

#define	asgn_trl_empty()	(AsgnTRP == AsgnTrl)
#define	chng_trl_empty()	(ChngTRP == ChngTrl)

/*
** Tables
*/

#define Def_TblsSize	30000*sizeof(heapT)
#define	Tbls_TH		10

#define	Sus_Address(P)		((heapP) (*(P)))

#define sus_tbl_empty()		(STP == SusTbl)

#define	err_tbl_empty()		(ERRP == ErrTbl)

/* 
**  Flags Tags and Values
*/

#define	FlagShift	 2
#define	CodeShift	 4
#define	TagShift	(FlagShift + CodeShift)

#define FlagBits	0x00000003
#define CodeBits	0x0000003c
#define TagBits         (FlagBits | CodeBits)
#define ValBits 	0xffffffc0

#define ValMask 	0x03ffffff

#define	Flag_of(V)	((V) & FlagBits)
#define	Code_of(V)	(((V) & CodeBits) >> FlagShift)
#define	Tag_of(V)	((V) & TagBits)
#define	Val_of(V)	((V) >> TagShift)

#define	Off_Tag(V)	((V) & (~TagBits))
#define	On_Tag(V, Tag)	((V) | (Tag))

#define	Tag(Code, Flag)		(((Code) << FlagShift) | (Flag))
#define	Word(Val, Tag)		((((heapT) (Val)) << TagShift) | (Tag))

/*
** Flags
*/

#define RefFlag		0x0
#define RegularFlag     0x1
#define ListFlag        0x2

/*
** References
*/

#define IsRef(V)        (Flag_of(V) == RefFlag)
#define Ref_Val(V)	((heapP) (V))
#define Ref_Word(V)	((heapT) (V))

/*
** Variables
*/

#define	VarShift	4
#define	VarValBits	0xffffffc0
#define	VarValMask	0x0ffffffc

#define ZeroedWrt	WrtTag

#define IsVar(V)	(((V) & 0x3b) == WrtTag)

#define Var_Word(Val, Tag)	((((heapT) (Val)) << VarShift) | (Tag))


#define HOByteMask	0xf0000000

unsigned int HOByte;
#define	HOPage	0x10000000

#define Var_Val(V)	((heapP) ((((V) >> VarShift) & VarValMask) | HOByte))

#define IsZeroed(V)	(((V) & VarValBits) == 0x0)

/* 
** Writable Variables
*/

#define WrtCode		0x0
#define WrtTag          Tag(WrtCode, RegularFlag)

#define IsWrt(V)	(((V) & 0x3f) == WrtTag)

/*
** Read Only Variables
*/

#define RoCode		0x1
#define RoTag           Tag(RoCode, RegularFlag)

#define IsRo(V)		(((V) & 0x3f) == RoTag)

/*
** Integers
*/

#define IntCode		0x2
#define IntTag          Tag(IntCode, RegularFlag)

#define IsInt(V)	(Tag_of(V) == IntTag)

#define MaxInt          0x01ffffff
#define MinInt         -0x01ffffff

#define	Int_Val(V)	(((int) (V)) >> TagShift)
 
/*
**  Reals
*/

#define RealCode	0x3
#define RealTag         Tag(RealCode, RegularFlag)

#define IsReal(V)       (Tag_of(V) == RealTag)

/*
**  Strings
*/

#define StrCode		0x4
#define StrTag          Tag(StrCode, RegularFlag)

#define IsStr(V)	(Tag_of(V) == StrTag)

#define MaxStr		0x00ffffff

#define StrOffsetShift	22
#define StrLengthShift	24

#define StrOffsetBits	0x0fffffc0
#define StrTypeMask	0x0000000f
#define	StrLengthBits	0x00ffffff
#define	StrHashMask	0x000000ff

#define Str_Type(P)	((((int) *(P)) >> \
			  (StrOffsetShift + TagShift)) & StrTypeMask)
#define Str_Offset(P)	((((int) *(P)) & StrOffsetBits) >> TagShift)

#define	Str_Hash(P)	((((int) *(P+1)) >> StrLengthShift) & StrHashMask)
#define	Str_Length(P)	(((int) *(P+1)) & StrLengthBits)

#define	Str_Words(P)	((Str_Length(P) + sizeof(heapT)) / sizeof(heapT))

#define Str_Hdr1(Type, Offset)	Word(((Type << StrOffsetShift) | Offset), \
				     StrTag)
#define Str_Hdr2(Hash, Length)	((heapT) ((((int) (Hash)) << StrLengthShift) \
					  | Length))

#define	str_total_words(CharsNum) 	\
	(((CharsNum  + sizeof(heapT)) / sizeof(heapT)) + StrHdrWords)

#define CharType	0x0
#define PrcdrType	0x1
#define MdlType		0x2
#define FrznType	0x3
#define CtlType		0x4

#define	StrHdrWords	2

#define CharHdrWords	2
#define PrcdrHdrWords	4
#define MdlHdrWords	3
#define FrznHdrWords	2
#define CtlHdrWords	3

/*
**  Nills
*/

#define NilCode		0x5
#define NilTag          Tag(NilCode, RegularFlag)

#define IsNil(V)	(Tag_of(V) == NilTag)

/*
**  Lists
*/

#define L_RefFlag       (ListFlag | RefFlag)

#define L_IntTag        (ListFlag | IntTag)
#define L_NilTag        (ListFlag | NilTag)

#define IsList(V)	((V) & ListFlag)
#define IsL_Ref(V)	(Flag_of(V) == L_RefFlag)
#define IsL_Int(V)	(Tag_of(V) == L_IntTag)
#define IsL_Nil(V)	(Tag_of(V) == L_NilTag)

#define L_Ref_Val(V)	((heapP) Off_List(V))
#define L_Ref_Word(V)	(((heapT) (V)) | L_RefFlag)

#define	Set_List(V)	((V) | ListFlag)
#define	Off_List(V)	((V) & (~ListFlag))

#define	Cdr(P)	(P + 1)

/*
**  Tuples
*/

#define TplCode		0x6
#define TplTag          Tag(TplCode, RegularFlag)

#define IsTpl(V)	(Tag_of(V) == TplTag)

#define	MaxTpl		MaxInt

#define Arity_of(V)	((int) Val_of(V))

/*
** Vectors
*/

#define VctrCode	0x7
#define VctrTag         Tag(VctrCode, RegularFlag)

#define IsVctr(V)	(Tag_of(V) == VctrTag)

#define	MaxVctr 	MaxInt

/*
** Invalid
*/

#define	InvldCode	0x8
#define	InvldTag	Tag(InvldCode, RegularFlag)

#define IsInvld(V)	(Tag_of(V) == InvldTag)

/*
** Tag Indexes
*/

#define	RefIndex	0x0
#define	WrtIndex	0x1
#define	RoIndex		0x3
#define	IntIndex	0x5
#define	RealIndex	0x6
#define	StrIndex	0x7
#define	NilIndex	0x8
#define	L_RefIndex	0x9
#define	L_IntIndex	0xa
#define	L_NilIndex	0xb
#define	TplIndex	0xc
#define	VctrIndex	0xd
#define InvldIndex	0xf
#define	DfltIndex	0xe

/*
**  Modules, Procedures
*/

#define Get_Info(P)	(P + *((int *) P))

#define Mdl_Info(Mdl)		(((int) *(Mdl+StrHdrWords) == 0) ? Nil : \
				 Get_Info((Mdl+StrHdrWords)))

/* @@@ Until we drop the old pilot we have to check if the module info is 
   a string */
#define Mdl_Name(Info)		(IsStr(*(Info)) ? \
				 ((char *) (Info + StrHdrWords)) : \
				 ((char *) Info))

/* Old Emulator */

#define Prcdr_Info(Prcdr)	(((int) *(Prcdr+StrHdrWords+1) == 0) ? Nil : \
				 Get_Info((Prcdr+StrHdrWords+1)))

#define Prcdr_Name(Info)	((char *) (Info+1))

#define Prcdr_Arity(Info)	((int) *Info)

/* New Emulator */

#define Ctl_Info(Ctl)		Mdl_Info(Ctl)

#define Ctl_Name(Info)		((char *) (Get_Info(Info) + StrHdrWords))

#define Ctl_Arity(Info)		((int) *(Info+1))

#define Ctl_Index(Info)		((int) *(Info+2))

/*
** Temporary Registers
*/
  
#define X_size		4096

/*
** Free Lists
*/

#define	FLsSize	(PR_Header + 256 + 1)

/*
**  Process records
*/

/* Process reocrds are variable sized vectors. Maximum number of arguments
   they can contain is 256. */

#define PR_Header	3

#define	Prcdr_PR(Process)	(Process + 1)
#define	Next_PR(Process)	(Process + 2)
#define	Args_PR(Process)	(Process + 3)
#define Index_PR(Process)	Args_PR(Process)
#define Native_Args_PR(Process)	(Process + 4)

#define ArgsNo_PR(Process)	(Arity_of(*Process) - (PR_Header-1))


/*
**  Suspension records and queues
*/

#define SR_Size		2

#define	Ref_SR(Suspension)	(Suspension)
#define	Next_SR(Suspension)	(Suspension + 1)

/*
** Shared Variable Records
*/

#define SVR_Arity	3
#define SVR_Size	(1 + SVR_Arity)

#define Marker_SVR(SVR)	(SVR+1)
#define	Type_SVR(SVR)	(SVR+2)
#define Id_SVR(SVR)	(SVR+3)

/*
**  Time Slice
*/

#define Def_TimeSlice		500
#define Def_DevsTime		676

#define ended_time_slice()	(TS == 0)

/*
** Min and Max Values
*/

#define	MaxChar		0xff
#define MaxShort	0xffff

/*
** Various Value Macros
*/

#define align(Addr, Size)	((char *) (Addr) + \
				 ((((int) (Addr) % (Size)) == 0) ? \
				 0 : ((Size) - ((int) (Addr) % (Size)))))

/*
** Worker
*/

typedef	struct { /* Worker */
heapT	X_W[X_size];	/* Temporary Registers */
heapP	HP_W;		/* Heap Pointer */
heapP	CP_W;		/* Current Process */
int	TS_W;		/* Time Slice counter */
trailP	AsgnTrl_W;	/* Writable Vars Assignment Trail */
trailP  AsgnTRP_W;	/* Writable Vars Assignment Trail Pointer */
trailP	ChngTrl_W;	/* Destructive Assignment Trail */
trailP  ChngTRP_W;	/* Destructive Assignment Trail Pointer */
heapP	SusTbl_W;	/* Suspension Table */
heapP	STP_W;		/* Suspension Table Pointer */
int	TempSus_W;	/* Suspension on Local Variable Flag */
heapP	HB_W;		/* Heap Backtrack */
heapP	QF_W;		/* Active Queue Front */
heapP	QB_W;		/* Active Queue Back */
heapP	SQF_W;		/* Suspended Queue Front */
heapP	SQB_W;		/* Suspended Queue Back */
int	HighQ_W;	/* High Queue Flag */
heapP	HQB_W;		/* High Priority Active Queue Back */
heapP 	FLs_W[FLsSize];	/* Free Lists */

linkP	LP_W;		/* Link Pointer */

heapP	ErrTbl_W;	/* Error Table */
heapP	ERRP_W;		/* Error Table Pointer */

int     TimeSlice_W;	/* starting Time Slice */
int	DevsTime_W;	/* Devices check Time interval */

int	Last_Redo_GC_W;	/* When last redo gc was done */

heapP	McnInP_W;	/* Machine Input Pointer */
heapP	McnOutP_W;	/* Machine Output Pointer */
int	McnOutM_W;	/* Machine Output Masks */

int	DevsNo_W;	/* Active Devices Number */

heapP   Nil_W;		/* Nil Constant */
heapP   SVRMarker_W;	/* Nil Constant */

int	TimeDelta_W;	/* Time Delta (Added to Reductions) */

heapT	KOutA_W;	/* Kernels Output A */
heapT	KOutB_W;	/* Kernels Output B */
heapT	KOutC_W;	/* Kernels Output C */

unsigned int	Creations_W;	/* no. of processes created */
unsigned int	Suspensions_W;	/* no. of processes suspended */
unsigned int	Activations_W;	/* no. of processes activated */
unsigned int	Failures_W;	/* no. of processes failed */
unsigned int	Losses_W;	/* no. of processes lost */
unsigned int	Switches_W;	/* no. of process switches */
unsigned int	Reductions_W;	/* no. of processes reduced */
unsigned int	Terminations_W;	/* no. of processes terminated */
unsigned int	CpuTime_W;	/* Total Cpu Time */
unsigned int	GCTime_W;	/* GC Time */
unsigned int	Collections_W;	/* no. of garbage collections */
unsigned int	CopiedAverage_W;/* Average of copied heap space */
unsigned int	FreedAverage_W;	/* Average of freed heap space */
unsigned int	GCMinFlt_W;	/* Minor Page Faults due to GC */
unsigned int	GCMajFlt_W;	/* Major Page Faults due to GC */


int StrOrder_W[(CtlType+1)];	/* Strings Types Order */

#ifdef	DEBUG
heapP	PrevSTP_W;	/* Previous STP */

int	Debug_Process_W;  	/* process debugging flag */
int	Debug_Clause_W;	  	/* clause debugging flag */
int	Debug_Guard_W;	  	/* guard debugging flag */
int	Debug_Outargs_W;	/* outargs debugging flag */
int	Debug_Activation_W;	/* process activation debugging flag */
int	Debug_W;		/* Emulator debugging flag */
int	Debug_Unset_W;  	/* unset debugging flags */
int	Debug_Buffered_W;  	/* buffered debugging flag */
#endif
} workerT;

typedef	workerT	*workerP;

