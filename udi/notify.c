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

#include	<stdio.h>
extern	FILE *DbgFile, *OutFile;

#include	<signal.h>
#include	<string.h>
#include	<errno.h>
#include	<sys/file.h>
#include	<sys/time.h>
#include	<time.h>
#ifdef	SUNOS4d1d3
#include	<sys/vadvise.h>
#endif
#include	<sys/resource.h>

#include	"fcp.h"
#include	"codes.h"
#include	"global.h"
#include	"macros.h"

extern void exit(int);

/* Init */

fcp_init_io()
{
  init_devices();
  init_signals();
#ifdef	SUNOS4d1d3
  vadvise(VA_ANOM); 	/* not a normal page behaviour */
#endif
  DbgFile = stderr;
  OutFile = stdout;

#ifdef	DEBUG
  {
    FILE *Fp;

    if (Debug_Buffered) {
      Fp = fopen("debug.logix", "w");
      if (Fp == NULL) {
	fprintf(DbgFile, "Can not open 'debug.logix'\n");
	Debug_Buffered = False;
      }
      else {
	fprintf(DbgFile, "Output is written to ./'debug.logix'\n");
	DbgFile = Fp;
      }
    }
  }
#endif
}

/* Exit */

int Exiting = False;
int SpeakerPid = 0;

int do_exit(Msg, Type, Value, Dump)
char *Msg;
int Type, Value, Dump;
{
  if (!Exiting) {
    fcp_reset_io();
    Exiting = True; /* Avoid looping by signals */
    print_reason(Msg, Type, Value);
    if (Dump) {
      stop_time();
      print_core(Msg, Type, Value);
    }
  }
  if (SpeakerPid > 0)
    kill(SpeakerPid, SIGKILL);
  if (Type == NORMAL) {
    exit(Value);
  }
  exit(Type);
}

fcp_reset_io()
{
#ifdef	DLVS
  dlvsExit();
#endif
  device_handlers(ExitC);
}

/* Device handling */

#define MaxDevices	FD_SETSIZE
#define MaxSignals	NSIG

typedef struct {
  int Active;
  int Selected;
  int (*Func)();
  int (*Handler)();
  heapP Strm;
  int Count;
  int Next;
  int Prev;
} DeviceT;

static DeviceT	ReadDevs[(MaxDevices+1)];
static DeviceT	WriteDevs[(MaxDevices+1)];
static DeviceT	ExceptDevs[(MaxDevices+1)];
static DeviceT	SignalDevs[(MaxSignals+1)];

init_devices()
{
  register int Entry;

  ReadDevs[MaxDevices].Next = ReadDevs[MaxDevices].Prev = MaxDevices;
  WriteDevs[MaxDevices].Next = WriteDevs[MaxDevices].Prev = MaxDevices;
  ExceptDevs[MaxDevices].Next = ExceptDevs[MaxDevices].Prev = MaxDevices;
  SignalDevs[MaxSignals].Next = SignalDevs[MaxSignals].Prev = MaxSignals;

  for (Entry = 0; Entry <= MaxDevices; Entry++) {
    ReadDevs[Entry].Active = False;
    WriteDevs[Entry].Active = False;
    ExceptDevs[Entry].Active = False;
  }
  for (Entry = 0; Entry <= MaxSignals; Entry++) {
    SignalDevs[Entry].Active = False;
  }

  DevsNo = 0;
}

int set_select_entry(SelectType, Entry, PVar, Func, Handler)
     int SelectType, Entry;
     heapP PVar;
     int (*Func)();
     int (*Handler)();
{
  register heapP P0 = PVar;

  deref_ptr(P0);
  if (!IsWrt(*P0)) {
    return(False);
  }
  switch (SelectType) {
  case ReadC:
    if ((Entry < 0) || (Entry >= MaxDevices)) {
      return(False);
    }
    if (ReadDevs[Entry].Active == True) {
      register heapP P = ReadDevs[Entry].Strm;

      deref_ptr(P);
      asgn(*P0, P0, Ref_Word(Var_Val(*P)));
    }
    else {
      ReadDevs[Entry].Active = True;
      ReadDevs[Entry].Selected = True;
      ReadDevs[Entry].Func = Func;
      ReadDevs[Entry].Handler = Handler;
      ReadDevs[Entry].Strm = HP;
      ReadDevs[Entry].Count = 0;
      asgn(*P0, P0, Var_Word(HP, RoTag));
      *HP++ = Var_Word(P0, WrtTag);
      ReadDevs[Entry].Next = MaxDevices;
      ReadDevs[Entry].Prev = ReadDevs[MaxDevices].Prev;
      ReadDevs[ReadDevs[MaxDevices].Prev].Next = Entry;
      ReadDevs[MaxDevices].Prev = Entry;
      DevsNo++;
    }
    break;
  case WriteC:
    if ((Entry < 0) || (Entry >= MaxDevices)) {
      return(False);
    }
    if (WriteDevs[Entry].Active == True) {
      register heapP P = WriteDevs[Entry].Strm;

      deref_ptr(P);
      asgn(*P0, P0, Ref_Word(Var_Val(*P)));
    }
    else {
      WriteDevs[Entry].Active = True;
      WriteDevs[Entry].Selected = True;
      WriteDevs[Entry].Func = Func;
      WriteDevs[Entry].Handler = Handler;
      WriteDevs[Entry].Strm = HP;
      WriteDevs[Entry].Count = 0;
      asgn(*P0, P0, Var_Word(HP, RoTag));
      *HP++ = Var_Word(P0, WrtTag);
      WriteDevs[Entry].Next = MaxDevices;
      WriteDevs[Entry].Prev = WriteDevs[MaxDevices].Prev;
      WriteDevs[WriteDevs[MaxDevices].Prev].Next = Entry;
      WriteDevs[MaxDevices].Prev = Entry;
      DevsNo++;
    }
    break;
  case ExceptC:
    if ((Entry < 0) || (Entry >= MaxDevices)) {
      return(False);
    }
    if (ExceptDevs[Entry].Active == True) {
      register heapP P = ExceptDevs[Entry].Strm;

      deref_ptr(P);
      asgn(*P0, P0, Ref_Word(Var_Val(*P)));
    }
    else {
      ExceptDevs[Entry].Active = True;
      ExceptDevs[Entry].Selected = True;
      ExceptDevs[Entry].Func = Func;
      ExceptDevs[Entry].Handler = Handler;
      ExceptDevs[Entry].Strm = HP;
      ExceptDevs[Entry].Count = 0;
      asgn(*P0, P0, Var_Word(HP, RoTag));
      *HP++ = Var_Word(P0, WrtTag);
      ExceptDevs[Entry].Next = MaxDevices;
      ExceptDevs[Entry].Prev = ExceptDevs[MaxDevices].Prev;
      ExceptDevs[ExceptDevs[MaxDevices].Prev].Next = Entry;
      ExceptDevs[MaxDevices].Prev = Entry;
      DevsNo++;
    }
    break;
  case SignalC:
    if ((Entry < 0) || (Entry >= MaxSignals)) {
      return(False);
    }
    if (SignalDevs[Entry].Active == True) {
      register heapP P = SignalDevs[Entry].Strm;

      deref_ptr(P);
      asgn(*P0, P0, Ref_Word(Var_Val(*P)));
    }
    else {
      SignalDevs[Entry].Active = True;
      SignalDevs[Entry].Selected = True;
      SignalDevs[Entry].Func = Func;
      SignalDevs[Entry].Handler = Handler;
      SignalDevs[Entry].Strm = HP;
      SignalDevs[Entry].Count = 0;
      asgn(*P0, P0, Var_Word(HP, RoTag));
      *HP++ = Var_Word(P0, WrtTag);
      SignalDevs[Entry].Next = MaxDevices;
      SignalDevs[Entry].Prev = SignalDevs[MaxDevices].Prev;
      SignalDevs[SignalDevs[MaxDevices].Prev].Next = Entry;
      SignalDevs[MaxDevices].Prev = Entry;
      DevsNo++;
    }
    break;
  default:
    return(False);
  }
  return(True);
}

int reset_select_entry(SelectType, Entry)
     int SelectType, Entry;
{
  switch (SelectType) {
  case ReadC:
    if ((Entry < 0) || (Entry >= MaxDevices)) {
      return(False);
    }
    if (ReadDevs[Entry].Active == True) {
      register heapP P = ReadDevs[Entry].Strm;

      deref_ptr(P);
      asgn(*P, P, Word(0, NilTag));
      ReadDevs[Entry].Active = False;
      ReadDevs[ReadDevs[Entry].Prev].Next = ReadDevs[Entry].Next;
      ReadDevs[ReadDevs[Entry].Next].Prev = ReadDevs[Entry].Prev;
      DevsNo--;
    }
    break;
  case WriteC:
    if ((Entry < 0) || (Entry >= MaxDevices)) {
      return(False);
    }
    if (WriteDevs[Entry].Active == True) {
      register heapP P = WriteDevs[Entry].Strm;

      deref_ptr(P);
      asgn(*P, P, Word(0, NilTag));
      WriteDevs[Entry].Active = False;
      WriteDevs[WriteDevs[Entry].Prev].Next = WriteDevs[Entry].Next;
      WriteDevs[WriteDevs[Entry].Next].Prev = WriteDevs[Entry].Prev;
      DevsNo--;
    }
    break;
  case ExceptC:
    if ((Entry < 0) || (Entry >= MaxDevices)) {
      return(False);
    }
    if (ExceptDevs[Entry].Active == True) {
      register heapP P = ExceptDevs[Entry].Strm;

      deref_ptr(P);
      asgn(*P, P, Word(0, NilTag));
      ExceptDevs[Entry].Active = False;
      ExceptDevs[ExceptDevs[Entry].Prev].Next = ExceptDevs[Entry].Next;
      ExceptDevs[ExceptDevs[Entry].Next].Prev = ExceptDevs[Entry].Prev;
      DevsNo--;
    }
    break;
  case SignalC:
    if ((Entry < 0) || (Entry >= MaxSignals)) {
      return(False);
    }
    if (SignalDevs[Entry].Active == True) {
      register heapP P = SignalDevs[Entry].Strm;

      deref_ptr(P);
      asgn(*P, P, Word(0, NilTag));
      SignalDevs[Entry].Active = False;
      SignalDevs[SignalDevs[Entry].Prev].Next = SignalDevs[Entry].Next;
      SignalDevs[SignalDevs[Entry].Next].Prev = SignalDevs[Entry].Prev;
      DevsNo--;
    }
    break;
  default:
    return(False);
  }
  return(True);
}

select_devices(NonBlocking)
     int NonBlocking;
{

  fd_set ReadSlcts, WriteSlcts, ExceptSlcts;
  fd_set *ReadSlctsP, *WriteSlctsP, *ExceptSlctsP;
  int DeviceSelected;
  int MaxSlctdDev;
  int MaxReadDev, MaxWriteDev, MaxExceptDev;

  int Selections;

  struct timeval TimeOut;

  register int Entry;

  ReadSlctsP = (fd_set *) &ReadSlcts;
  FD_ZERO(ReadSlctsP);
  WriteSlctsP = (fd_set *) &WriteSlcts;
  FD_ZERO(WriteSlctsP);
  ExceptSlctsP = (fd_set *) &ExceptSlcts;
  FD_ZERO(ExceptSlctsP);

  DeviceSelected = False;
  MaxSlctdDev = 0;
  MaxReadDev = MaxWriteDev = MaxExceptDev = 0;

  Entry = MaxDevices;
  while (ReadDevs[Entry].Next != MaxDevices) {
    Entry = ReadDevs[Entry].Next;
    if (ReadDevs[Entry].Selected == True) {
      FD_SET(Entry, ReadSlctsP);
      DeviceSelected = True;
      if (MaxSlctdDev < Entry) {
	MaxSlctdDev = Entry;
      }
      if (MaxReadDev < Entry) {
	MaxReadDev = Entry;
      }
    }
  }
    
  Entry = MaxDevices;
  while (WriteDevs[Entry].Next != MaxDevices) {
    Entry = WriteDevs[Entry].Next;
    if (WriteDevs[Entry].Selected == True) {
      FD_SET(Entry, WriteSlctsP);
      DeviceSelected = True;
      if (MaxSlctdDev < Entry) {
	MaxSlctdDev = Entry;
      }
      if (MaxWriteDev < Entry) {
	MaxWriteDev = Entry;
      }
    }
  }

  Entry = MaxDevices;
  while (ExceptDevs[Entry].Next != MaxDevices) {
    Entry = ExceptDevs[Entry].Next;
    if (ExceptDevs[Entry].Selected == True) {
      FD_SET(Entry, ExceptSlctsP);
      DeviceSelected = True;
      if (MaxSlctdDev < Entry) {
	MaxSlctdDev = Entry;
      }
      if (MaxExceptDev < Entry) {
	MaxExceptDev = Entry;
      }
    }
  }

  if (DeviceSelected == True) {
    if (NonBlocking) {
      TimeOut.tv_sec = 0;
      TimeOut.tv_usec = 0;
    }
    else {
      TimeOut.tv_sec = 0x7fffff;
      TimeOut.tv_usec = 0;
    }

#ifdef	HPUX
    Selections = select((MaxSlctdDev+1),
			((int *) ReadSlctsP),
			((int *) WriteSlctsP),
			((int *) ExceptSlctsP),
			&TimeOut);
#else
    Selections = select((MaxSlctdDev+1), ReadSlctsP, WriteSlctsP, ExceptSlctsP,
			&TimeOut);
#endif

    if (Selections > 0) {
      for (Entry = 0; Entry <= MaxSlctdDev; Entry++) {

	if ((Entry <= MaxReadDev) && (FD_ISSET(Entry, ReadSlctsP))) {
	  ReadDevs[Entry].Selected = False;
	  if ((ReadDevs[Entry].Func) != ((int (*)()) Null)) {
	    (*(ReadDevs[Entry].Func))(Entry, ReadC);
	  }
	  else {
	    ReadDevs[Entry].Strm =
	      new_list_element(ReadDevs[Entry].Strm, Constants[ReadC]);
	  }
	}
	
	if ((Entry <= MaxWriteDev) && (FD_ISSET(Entry, WriteSlctsP))) {
	  WriteDevs[Entry].Selected = False;
	  if ((WriteDevs[Entry].Func) != ((int (*)()) Null)) {
	    (*(WriteDevs[Entry].Func))(Entry, WriteC);
	  }
	  else {
	    WriteDevs[Entry].Strm =
	      new_list_element(WriteDevs[Entry].Strm, Constants[WriteC]);
	  }
	}
	
	if ((Entry <= MaxExceptDev) && (FD_ISSET(Entry, ExceptSlctsP))) {
	  ExceptDevs[Entry].Selected = False;
	  if ((ExceptDevs[Entry].Func) != ((int (*)()) Null)) {
	    (*(ExceptDevs[Entry].Func))(Entry, ExceptC);
	  }
	  else {
	    ExceptDevs[Entry].Strm =
	      new_list_element(ExceptDevs[Entry].Strm, Constants[ExceptC]);
	  }
	}
      }
    }
  }

  Entry = MaxSignals;
  while (SignalDevs[Entry].Next != MaxSignals) {
    Entry = SignalDevs[Entry].Next;
    if (SignalDevs[Entry].Selected == True) {
      if (SignalDevs[Entry].Count > 0) {
	SignalDevs[Entry].Selected = False;
	if ((SignalDevs[Entry].Func) != ((int (*)()) Null)) {
	  (*(SignalDevs[Entry].Func))(Entry, SignalC);
	}
	else {
	  SignalDevs[Entry].Strm =
	    new_list_element(SignalDevs[Entry].Strm, Constants[SignalC]);
	}
      }
    }
  }

  HB = HP;
}

add_to_device_stream(SelectType, Entry, Element)
     int SelectType;
     int Entry;
     heapP Element;
{
  switch (SelectType) {
  case ReadC:
    if ((Entry < 0) || (Entry > MaxDevices)) {
      return(False);
    }
    if (ReadDevs[Entry].Active != True) {
      return(False);
    }
    ReadDevs[Entry].Strm = new_list_element(ReadDevs[Entry].Strm, Element);
    return(True);
  case WriteC:
    if ((Entry < 0) || (Entry > MaxDevices)) {
      return(False);
    }
    if (WriteDevs[Entry].Active != True) {
      return(False);
    }
    WriteDevs[Entry].Strm = new_list_element(WriteDevs[Entry].Strm, Element);
    return(True);
  case ExceptC:
    if ((Entry < 0) || (Entry > MaxDevices)) {
      return(False);
    }
    if (ExceptDevs[Entry].Active != True) {
      return(False);
    }
    ExceptDevs[Entry].Strm = new_list_element(ExceptDevs[Entry].Strm, Element);
    return(True);
  case SignalC:
    if ((Entry < 0) || (Entry > MaxSignals)) {
      return(False);
    }
    if (SignalDevs[Entry].Active != True) {
      return(False);
    }
    SignalDevs[Entry].Strm = new_list_element(SignalDevs[Entry].Strm, Element);
    return(True);
  default:
    return(False);
  }
}

int next_select_token(SelectType, Entry)
     int SelectType, Entry;
{
  switch (SelectType) {
  case ReadC:
    ReadDevs[Entry].Selected = True;
    return;
  case WriteC:
    WriteDevs[Entry].Selected = True;
    return;
  case ExceptC:
    ExceptDevs[Entry].Selected = True;
    return;
  case SignalC:
    SignalDevs[Entry].Selected = True;
    return;
  }
}

int read_device_count(SelectType, Entry)
     int SelectType, Entry;
{
  register int Count;

  switch (SelectType) {
  case ReadC:
    Count = ReadDevs[Entry].Count;
    ReadDevs[Entry].Count -= Count;
    return(Count);
  case WriteC:
    Count = WriteDevs[Entry].Count;
    WriteDevs[Entry].Count -= Count;
    return(Count);
  case ExceptC:
    Count = ExceptDevs[Entry].Count;
    ExceptDevs[Entry].Count -= Count;
    return(Count);
  case SignalC:
    Count = SignalDevs[Entry].Count;
    SignalDevs[Entry].Count -= Count;
    return(Count);
  }
}

device_handlers(Event)
     int Event;
{
  register int Entry;

  Entry = MaxDevices;
  while (ReadDevs[Entry].Next != MaxDevices) {
    Entry = ReadDevs[Entry].Next;
    if (ReadDevs[Entry].Handler != ((int (*)()) Null)) {
      (*(ReadDevs[Entry].Handler))(Event);
    }
  }
    
  Entry = MaxDevices;
  while (WriteDevs[Entry].Next != MaxDevices) {
    Entry = WriteDevs[Entry].Next;
    if (WriteDevs[Entry].Handler != ((int (*)()) Null)) {
      (*(WriteDevs[Entry].Handler))(Event);
    }
  }

  Entry = MaxDevices;
  while (ExceptDevs[Entry].Next != MaxDevices) {
    Entry = ExceptDevs[Entry].Next;
    if (ExceptDevs[Entry].Handler != ((int (*)()) Null)) {
      (*(ExceptDevs[Entry].Handler))(Event);
    }
  }

  Entry = MaxSignals;
  while (SignalDevs[Entry].Next != MaxSignals) {
    Entry = SignalDevs[Entry].Next;
    if (SignalDevs[Entry].Handler != ((int (*)()) Null)) {
      (*(SignalDevs[Entry].Handler))(Event);
    }
  }
}

int waiting_to_devices()
{
  register int Entry;

  Entry = MaxDevices;
  while (ReadDevs[Entry].Next != MaxDevices) {
    Entry = ReadDevs[Entry].Next;
    if (waiting_to(ReadDevs[Entry].Strm)) {
      return(True);
    }
  }
    
  Entry = MaxDevices;
  while (WriteDevs[Entry].Next != MaxDevices) {
    Entry = WriteDevs[Entry].Next;
    if (waiting_to(WriteDevs[Entry].Strm)) {
      return(True);
    }
  }

  Entry = MaxDevices;
  while (ExceptDevs[Entry].Next != MaxDevices) {
    Entry = ExceptDevs[Entry].Next;
    if (waiting_to(ExceptDevs[Entry].Strm)) {
      return(True);
    }
  }

  Entry = MaxSignals;
  while (SignalDevs[Entry].Next != MaxSignals) {
    Entry = SignalDevs[Entry].Next;
    if (waiting_to(SignalDevs[Entry].Strm)) {
      return(True);
    }
  }
  return(False);
}

prepare_devices_streams_for_gc()
{
  register int Entry;
  register heapP P;

  Entry = MaxDevices;
  while (ReadDevs[Entry].Next != MaxDevices) {
    Entry = ReadDevs[Entry].Next;
    P = ReadDevs[Entry].Strm;
    deref_ptr(P);
    ReadDevs[Entry].Strm = P;
    *HP++ = Ref_Word(P);
  }
    
  Entry = MaxDevices;
  while (WriteDevs[Entry].Next != MaxDevices) {
    Entry = WriteDevs[Entry].Next;
    P = WriteDevs[Entry].Strm;
    deref_ptr(P);
    WriteDevs[Entry].Strm = P;
    *HP++ = Ref_Word(P);
  }

  Entry = MaxDevices;
  while (ExceptDevs[Entry].Next != MaxDevices) {
    Entry = ExceptDevs[Entry].Next;
    P = ExceptDevs[Entry].Strm;
    deref_ptr(P);
    ExceptDevs[Entry].Strm = P;
    *HP++ = Ref_Word(P);
  }

  Entry = MaxSignals;
  while (SignalDevs[Entry].Next != MaxSignals) {
    Entry = SignalDevs[Entry].Next;
    P = SignalDevs[Entry].Strm;
    deref_ptr(P);
    SignalDevs[Entry].Strm = P;
    *HP++ = Ref_Word(P);
  }
}

adjust_devices_streams_after_gc()
{
  register int Entry;

  Entry = MaxDevices;
  while (ReadDevs[Entry].Next != MaxDevices) {
    Entry = ReadDevs[Entry].Next;
    ReadDevs[Entry].Strm = Ref_Val(*(ReadDevs[Entry].Strm));
  }
    
  Entry = MaxDevices;
  while (WriteDevs[Entry].Next != MaxDevices) {
    Entry = WriteDevs[Entry].Next;
    WriteDevs[Entry].Strm = Ref_Val(*(WriteDevs[Entry].Strm));
  }

  Entry = MaxDevices;
  while (ExceptDevs[Entry].Next != MaxDevices) {
    Entry = ExceptDevs[Entry].Next;
    ExceptDevs[Entry].Strm = Ref_Val(*(ExceptDevs[Entry].Strm));
  }

  Entry = MaxSignals;
  while (SignalDevs[Entry].Next != MaxSignals) {
    Entry = SignalDevs[Entry].Next;
    SignalDevs[Entry].Strm = Ref_Val(*(SignalDevs[Entry].Strm));
  }
}

/* Signals */

int CoreDumped = False;

static sigset_t NormalMask, ResumeMask, PauseMask, DumpMask, ExitMask;

#ifdef	SGI

set_signals_masks()
{
  sigemptyset(((sigset_t *) &NormalMask));
  sigaddset(((sigset_t *) &NormalMask), SIGURG);
  sigaddset(((sigset_t *) &NormalMask), SIGCHLD);
  sigaddset(((sigset_t *) &NormalMask), SIGIO);
  sigaddset(((sigset_t *) &NormalMask), SIGWINCH);

  ResumeMask = NormalMask;
  sigaddset(((sigset_t *) &ResumeMask), SIGCONT);

  PauseMask = ResumeMask;
  sigaddset(((sigset_t *) &PauseMask), SIGTSTP);
  sigaddset(((sigset_t *) &PauseMask), SIGTTIN);
  sigaddset(((sigset_t *) &PauseMask), SIGTTOU);

  DumpMask = PauseMask;
  sigaddset(((sigset_t *) &DumpMask), SIGQUIT);
  sigaddset(((sigset_t *) &DumpMask), SIGILL);
  sigaddset(((sigset_t *) &DumpMask), SIGTRAP);
  sigaddset(((sigset_t *) &DumpMask), SIGABRT);
  sigaddset(((sigset_t *) &DumpMask), SIGEMT);
  sigaddset(((sigset_t *) &DumpMask), SIGFPE);
  sigaddset(((sigset_t *) &DumpMask), SIGBUS);
  sigaddset(((sigset_t *) &DumpMask), SIGSEGV);
  sigaddset(((sigset_t *) &DumpMask), SIGSYS);

  ExitMask = DumpMask;
  sigaddset(((sigset_t *) &ExitMask), SIGHUP);
  sigaddset(((sigset_t *) &ExitMask), SIGINT);
  sigaddset(((sigset_t *) &ExitMask), SIGPIPE);
  sigaddset(((sigset_t *) &ExitMask), SIGALRM);
  sigaddset(((sigset_t *) &ExitMask), SIGTERM);
  sigaddset(((sigset_t *) &ExitMask), SIGUSR1);
  sigaddset(((sigset_t *) &ExitMask), SIGUSR2);
  sigaddset(((sigset_t *) &ExitMask), SIGPOLL);
  sigaddset(((sigset_t *) &ExitMask), SIGVTALRM);
  sigaddset(((sigset_t *) &ExitMask), SIGPROF);
  sigaddset(((sigset_t *) &ExitMask), SIGXCPU);
  sigaddset(((sigset_t *) &ExitMask), SIGXFSZ);

}

#endif

#ifdef	SUNOS4d1d3

set_signals_masks()
{
  sigemptyset(((sigset_t *) &NormalMask));
  sigaddset(((sigset_t *) &NormalMask), SIGURG);
  sigaddset(((sigset_t *) &NormalMask), SIGCHLD);
  sigaddset(((sigset_t *) &NormalMask), SIGIO);
  sigaddset(((sigset_t *) &NormalMask), SIGWINCH);

  ResumeMask = NormalMask;
  sigaddset(((sigset_t *) &ResumeMask), SIGCONT);

  PauseMask = ResumeMask;
  sigaddset(((sigset_t *) &PauseMask), SIGTSTP);
  sigaddset(((sigset_t *) &PauseMask), SIGTTIN);
  sigaddset(((sigset_t *) &PauseMask), SIGTTOU);

  DumpMask = PauseMask;
  sigaddset(((sigset_t *) &DumpMask), SIGQUIT);
  sigaddset(((sigset_t *) &DumpMask), SIGILL);
  sigaddset(((sigset_t *) &DumpMask), SIGTRAP);
  sigaddset(((sigset_t *) &DumpMask), SIGABRT);
  sigaddset(((sigset_t *) &DumpMask), SIGEMT);
  sigaddset(((sigset_t *) &DumpMask), SIGFPE);
  sigaddset(((sigset_t *) &DumpMask), SIGBUS);
  sigaddset(((sigset_t *) &DumpMask), SIGSEGV);
  sigaddset(((sigset_t *) &DumpMask), SIGSYS);
  sigaddset(((sigset_t *) &DumpMask), SIGLOST);

  ExitMask = DumpMask;
  sigaddset(((sigset_t *) &ExitMask), SIGHUP);
  sigaddset(((sigset_t *) &ExitMask), SIGINT);
  sigaddset(((sigset_t *) &ExitMask), SIGPIPE);
  sigaddset(((sigset_t *) &ExitMask), SIGALRM);
  sigaddset(((sigset_t *) &ExitMask), SIGTERM);
  sigaddset(((sigset_t *) &ExitMask), SIGXCPU);
  sigaddset(((sigset_t *) &ExitMask), SIGXFSZ);
  sigaddset(((sigset_t *) &ExitMask), SIGVTALRM);
  sigaddset(((sigset_t *) &ExitMask), SIGPROF);
  sigaddset(((sigset_t *) &ExitMask), SIGUSR1);
  sigaddset(((sigset_t *) &ExitMask), SIGUSR2);

}

#endif

#ifndef MACINTOSH
#ifdef	SUNOS5d3

set_signals_masks()
{
  sigemptyset(((sigset_t *) &NormalMask));
  sigaddset(((sigset_t *) &NormalMask), SIGCHLD);
  sigaddset(((sigset_t *) &NormalMask), SIGURG);
  sigaddset(((sigset_t *) &NormalMask), SIGIO);
  sigaddset(((sigset_t *) &NormalMask), SIGWINCH);

  ResumeMask = NormalMask;
  sigaddset(((sigset_t *) &ResumeMask), SIGPWR);
  sigaddset(((sigset_t *) &ResumeMask), SIGCONT);

  PauseMask = ResumeMask;
  sigaddset(((sigset_t *) &PauseMask), SIGTSTP);
  sigaddset(((sigset_t *) &PauseMask), SIGTTIN);
  sigaddset(((sigset_t *) &PauseMask), SIGTTOU);

  DumpMask = PauseMask;
  sigaddset(((sigset_t *) &DumpMask), SIGQUIT);
  sigaddset(((sigset_t *) &DumpMask), SIGILL);
  sigaddset(((sigset_t *) &DumpMask), SIGTRAP);
  sigaddset(((sigset_t *) &DumpMask), SIGABRT);
  sigaddset(((sigset_t *) &DumpMask), SIGEMT);
  sigaddset(((sigset_t *) &DumpMask), SIGFPE);
  sigaddset(((sigset_t *) &DumpMask), SIGBUS);
  sigaddset(((sigset_t *) &DumpMask), SIGSEGV);
  sigaddset(((sigset_t *) &DumpMask), SIGSYS);

  ExitMask = DumpMask;
  sigaddset(((sigset_t *) &ExitMask), SIGHUP);
  sigaddset(((sigset_t *) &ExitMask), SIGINT);
  sigaddset(((sigset_t *) &ExitMask), SIGPIPE);
  sigaddset(((sigset_t *) &ExitMask), SIGALRM);
  sigaddset(((sigset_t *) &ExitMask), SIGTERM);
  sigaddset(((sigset_t *) &ExitMask), SIGUSR1);
  sigaddset(((sigset_t *) &ExitMask), SIGUSR2);
  sigaddset(((sigset_t *) &ExitMask), SIGVTALRM);
  sigaddset(((sigset_t *) &ExitMask), SIGPROF);
  sigaddset(((sigset_t *) &ExitMask), SIGXCPU);
  sigaddset(((sigset_t *) &ExitMask), SIGXFSZ);
  sigaddset(((sigset_t *) &ExitMask), SIGWAITING);
  sigaddset(((sigset_t *) &ExitMask), SIGLWP);
  sigaddset(((sigset_t *) &ExitMask), SIGFREEZE);

  {
    register int Sig;

    for (Sig = _SIGRTMIN; Sig <= _SIGRTMAX; Sig++) {
      sigaddset(((sigset_t *) &ExitMask), Sig);
    }
  }
}

#endif
#endif

#ifdef	HPUX

set_signals_masks()
{
  sigemptyset(((sigset_t *) &NormalMask));
  sigaddset(((sigset_t *) &NormalMask), SIGCHLD);
  sigaddset(((sigset_t *) &NormalMask), SIGIO);
  sigaddset(((sigset_t *) &NormalMask), SIGWINCH);
  sigaddset(((sigset_t *) &NormalMask), SIGURG);

  ResumeMask = NormalMask;
  sigaddset(((sigset_t *) &ResumeMask), SIGPWR);
  sigaddset(((sigset_t *) &ResumeMask), SIGCONT);

  PauseMask = ResumeMask;
  sigaddset(((sigset_t *) &PauseMask), SIGTSTP);
  sigaddset(((sigset_t *) &PauseMask), SIGTTIN);
  sigaddset(((sigset_t *) &PauseMask), SIGTTOU);

  DumpMask = PauseMask;
  sigaddset(((sigset_t *) &DumpMask), SIGQUIT);
  sigaddset(((sigset_t *) &DumpMask), SIGILL);
  sigaddset(((sigset_t *) &DumpMask), SIGTRAP);
  sigaddset(((sigset_t *) &DumpMask), SIGABRT);
  sigaddset(((sigset_t *) &DumpMask), SIGEMT);
  sigaddset(((sigset_t *) &DumpMask), SIGFPE);
  sigaddset(((sigset_t *) &DumpMask), SIGBUS);
  sigaddset(((sigset_t *) &DumpMask), SIGSEGV);
  sigaddset(((sigset_t *) &DumpMask), SIGSYS);
  sigaddset(((sigset_t *) &DumpMask), SIGLOST);

  ExitMask = DumpMask;
  sigaddset(((sigset_t *) &ExitMask), SIGHUP);
  sigaddset(((sigset_t *) &ExitMask), SIGINT);
  sigaddset(((sigset_t *) &ExitMask), SIGPIPE);
  sigaddset(((sigset_t *) &ExitMask), SIGALRM);
  sigaddset(((sigset_t *) &ExitMask), SIGTERM);
  sigaddset(((sigset_t *) &ExitMask), SIGUSR1);
  sigaddset(((sigset_t *) &ExitMask), SIGUSR2);
  sigaddset(((sigset_t *) &ExitMask), SIGVTALRM);
  sigaddset(((sigset_t *) &ExitMask), SIGPROF);
  sigaddset(((sigset_t *) &ExitMask), _SIGRESERVE);
  sigaddset(((sigset_t *) &ExitMask), SIGDIL);
 
}

#endif

#if (defined MACINTOSH) || (defined LINUX) || (defined CYGWIN)

set_signals_masks()
{
  sigemptyset(((sigset_t *) &NormalMask));
  sigaddset(((sigset_t *) &NormalMask), SIGURG);
  sigaddset(((sigset_t *) &NormalMask), SIGCHLD);
  sigaddset(((sigset_t *) &NormalMask), SIGIO);
  sigaddset(((sigset_t *) &NormalMask), SIGWINCH);

  ResumeMask = NormalMask;
  sigaddset(((sigset_t *) &ResumeMask), SIGCONT);

  PauseMask = ResumeMask;
  sigaddset(((sigset_t *) &PauseMask), SIGTSTP);
  sigaddset(((sigset_t *) &PauseMask), SIGTTIN);
  sigaddset(((sigset_t *) &PauseMask), SIGTTOU);

  DumpMask = PauseMask;
  sigaddset(((sigset_t *) &DumpMask), SIGQUIT);
  sigaddset(((sigset_t *) &DumpMask), SIGILL);
  sigaddset(((sigset_t *) &DumpMask), SIGTRAP);
  sigaddset(((sigset_t *) &DumpMask), SIGABRT);
  /* sigaddset(((sigset_t *) &DumpMask), SIGEMT); */
  sigaddset(((sigset_t *) &DumpMask), SIGFPE);
  sigaddset(((sigset_t *) &DumpMask), SIGBUS);
  sigaddset(((sigset_t *) &DumpMask), SIGSEGV);
  /* sigaddset(((sigset_t *) &DumpMask), SIGSYS); */
  /* sigaddset(((sigset_t *) &DumpMask), SIGLOST); */

  ExitMask = DumpMask;
  sigaddset(((sigset_t *) &ExitMask), SIGHUP);
  sigaddset(((sigset_t *) &ExitMask), SIGINT);
  sigaddset(((sigset_t *) &ExitMask), SIGPIPE);
  sigaddset(((sigset_t *) &ExitMask), SIGALRM);
  sigaddset(((sigset_t *) &ExitMask), SIGTERM);
  sigaddset(((sigset_t *) &ExitMask), SIGXCPU);
  sigaddset(((sigset_t *) &ExitMask), SIGXFSZ);
  sigaddset(((sigset_t *) &ExitMask), SIGVTALRM);
  sigaddset(((sigset_t *) &ExitMask), SIGPROF);
  sigaddset(((sigset_t *) &ExitMask), SIGUSR1);
  sigaddset(((sigset_t *) &ExitMask), SIGUSR2);

}

#endif

static struct sigaction SignalVctr[unsignedbits];

int signal_event();

init_signals()
{
  sigset_t FullSet;

  int Sig;
  struct sigaction PAction;

  sigfillset(((sigset_t *) &FullSet));

  set_signals_masks();

  Sig = 1;
  while ((Sig < NSIG) && sigismember(((sigset_t *) &FullSet), Sig)) {
    PAction.sa_handler = (void (*)()) signal_event;
    if (sigismember(((sigset_t *) &NormalMask), Sig)) {
      PAction.sa_mask = NormalMask;
    }
    else {
      if (sigismember(((sigset_t *) &ResumeMask), Sig)) {
	PAction.sa_mask = ResumeMask;
      }
      else {
	if (sigismember(((sigset_t *) &PauseMask), Sig)) {
	  PAction.sa_mask = PauseMask;
	}
	else {
	  if (sigismember(((sigset_t *) &DumpMask), Sig)) {
	    PAction.sa_mask = DumpMask;
	  }
	  else {
	    if (sigismember(((sigset_t *) &ExitMask), Sig)) {
	      PAction.sa_mask = ExitMask;
	    }
	  }
	}
      }
      PAction.sa_flags = 0;
      sigaction(Sig, &PAction, ((struct sigaction *) 0));
    }
    Sig++;
  }
}

int signal_event(Sig)
     int Sig;
{
  if (SignalDevs[Sig].Active == True) {
    SignalDevs[Sig].Count++;
    return;
  }
  
  if (sigismember(((sigset_t *) &NormalMask), Sig)) {
    return;
  }
  else {
    if (sigismember(((sigset_t *) &ResumeMask), Sig)) {
      device_handlers(ResumeC);
      return;
    }
    else {
      if (sigismember(((sigset_t *) &PauseMask), Sig)) {
	device_handlers(PauseC);
	kill(getpid(), SIGSTOP);
	return;
      }
      else {
	if (sigismember(((sigset_t *) &DumpMask), Sig)) {
	  switch (Sig) {
	  case SIGQUIT:
#ifdef	DEBUG
	    if (Debug_Process && Debug_Clause && Debug_Guard &&
		Debug_Outargs && Debug_Activation) {
	      Debug_Process = False;
	      Debug_Clause = False;
	      Debug_Guard = False;
	      Debug_Outargs = False;
	      Debug_Activation = False;
	    }
	    else {
	      Debug_Process = True;
	      Debug_Clause = True;
	      Debug_Guard = True;
	      Debug_Outargs = True;
	      Debug_Activation = True;
	    }
	    return;
#endif
	  case SIGBUS:
	  case SIGSEGV:
#ifdef	DEBUG
	    if (!CoreDumped) {
	      char S[100];
	      
	      sprintf(S, "gcore -o fcp_core %d", getpid());
    
	      fprintf(DbgFile, "%s\n", S);
	      system(S);
	      CoreDumped = True;
	    }
#endif
	    break;
	  }

	  if (!Exiting) {
	    do_exit("", SIGNAL, Sig, True);
	  }
	}
	else {
	  if (sigismember(((sigset_t *) &ExitMask), Sig)) {
	    if (Sig == SIGUSR2) {
	      print_core("Core Requested","", "");
	      return;
	    }
	    if (!Exiting) {
	      do_exit("", SIGNAL, Sig, False);
	    }
	  }
	}
      }
    }
  }
}

/* Statistics. Adopted from Jaakov Levy. */


static struct rusage	R_Start, R_End;

start_time()
{
  extern int errno;

  if (getrusage(RUSAGE_SELF, &R_Start) < 0) {
    do_exit("getrusage - RUSAGE_SELF", SYSTEM, errno, False);
  }
}

stop_time()
{
  extern int errno;
  
  if (getrusage(RUSAGE_SELF, &R_End) < 0) {
    do_exit("getrusage - RUSAGE_SELF", SYSTEM, errno, False);
  }
  CpuTime += R_End.ru_utime.tv_usec - R_Start.ru_utime.tv_usec;
  CpuTime += (R_End.ru_utime.tv_sec - R_Start.ru_utime.tv_sec) * 1000000;

  if (CpuTime == 0) {
    CpuTime = 1; /* At least 1 microsecond..	*/
  }
}

