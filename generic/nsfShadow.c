/*
 * nsfShadow.c --
 *
 *      API support for shadowing (overloading) and accessing C-implemented
 *      Tcl obj-commands.
 *
 * Copyright (C) 1999-2017 Gustaf Neumann
 * Copyright (C) 2019      Stefan Sobernig
 * Copyright (C) 2020      Nathan Coulter
 *
 * Vienna University of Economics and Business
 * Institute of Information Systems and New Media
 * A-1020, Welthandelsplatz 1
 * Vienna, Austria
 *
 * This work is licensed under the MIT License https://www.opensource.org/licenses/MIT
 *
 * Copyright:
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 */

#include "nsfInt.h"
#include "nsfAccessInt.h"
#include "nsfCmdPtr.c"

static Tcl_ObjCmdProc Nsf_InfoFrameObjCmd;
EXTERN Tcl_ObjCmdProc NsfProcStub;
static Tcl_ObjCmdProc Nsf_InfoBodyObjCmd;
static Tcl_ObjCmdProc Nsf_RenameObjCmd;

/*
 *----------------------------------------------------------------------
 * NsfReplaceCommandCleanup --
 *
 *    Undoes the effects of NsfReplaceCommand() for the named Tcl command.
 *
 * Results:
 *    A Tcl return code.
 *
 * Side effects:
 *    None.
 *
 *----------------------------------------------------------------------
 */

int
NsfReplaceCommandCleanup(Tcl_Interp *interp, Tcl_Obj *nameObj, NsfShadowTclCommandInfo *ti) {
  Tcl_Command  cmd;
  int          result = TCL_OK;

  nonnull_assert(interp != NULL);
  nonnull_assert(nameObj != NULL);
  nonnull_assert(ti != NULL);

  /*fprintf(stderr, " cleanup for %s  ti=%p in %p\n", NsfGlobalStrings[name], ti, interp);*/
  cmd = Tcl_GetCommandFromObj(interp, nameObj);
  if (cmd != NULL) {
    Tcl_Command_objProc(cmd) = ti->proc;
    if (ti->clientData != NULL) {
      Tcl_Command_objClientData(cmd) = ti->clientData;
    }
    ti->proc = NULL;
    ti->clientData = NULL;
  } else {
    result = TCL_ERROR;
  }

  return result;
}

/*
 *----------------------------------------------------------------------
 * NsfReplaceCommandCheck --
 *
 *    Refreshes the wrapper command if needed and if overriding of Tcl commands
 *    is in effect.
 *
 * Results:
 *    A Tcl return code.
 *
 * Side effects:
 *    None.
 *
 *----------------------------------------------------------------------
 */
static void NsfReplaceCommandCheck(Tcl_Interp *interp, Tcl_Obj *nameObj, Tcl_ObjCmdProc *proc,
                                   NsfShadowTclCommandInfo *ti)
  nonnull(1) nonnull(2) nonnull(3) nonnull(4);

static void
NsfReplaceCommandCheck(Tcl_Interp *interp, Tcl_Obj *nameObj, Tcl_ObjCmdProc *proc,
                       NsfShadowTclCommandInfo *ti) {
  Tcl_Command cmd;

  nonnull_assert(interp != NULL);
  nonnull_assert(nameObj != NULL);
  nonnull_assert(proc != NULL);
  nonnull_assert(ti != NULL);

  cmd = Tcl_GetCommandFromObj(interp, nameObj);

  if (cmd != NULL && ti->proc && Tcl_Command_objProc(cmd) != proc) {
    /*
    fprintf(stderr, "we have to do something about %s %p %p\n",
	    NsfGlobalStrings[name], Tcl_Command_objProc(cmd), proc);
    */
    ti->proc = Tcl_Command_objProc(cmd);
    ti->clientData = Tcl_Command_objClientData(cmd);
    Tcl_Command_objProc(cmd) = proc;
  }
}

/*
 *----------------------------------------------------------------------
 * NsfReplaceCommand --
 *
 *    Looks up and stores the objProc of a Tcl command for efficient calling,
 *    optionally replacing the objProc with the given replacement.
 *
 * Results:
 *    A Tcl return code.
 *
 * Side effects:
 *    None.
 *
 *----------------------------------------------------------------------
 */
int
NsfReplaceCommand(Tcl_Interp *interp, Tcl_Obj *nameObj,
                  Tcl_ObjCmdProc *nsfReplacementProc,
                  ClientData cd,
                  NsfShadowTclCommandInfo *ti) {
  Tcl_Command cmd;
  int result = TCL_OK;

  nonnull_assert(interp != NULL);
  nonnull_assert(nameObj != NULL);
  nonnull_assert(ti != NULL);

  /* fprintf(stderr, "NsfReplaceCommand %s\n", ObjStr(nameObj)); */
  cmd = Tcl_GetCommandFromObj(interp, nameObj);

  if (cmd == NULL) {
    result = TCL_ERROR;
  } else {
    Tcl_ObjCmdProc *objProc = Tcl_Command_objProc(cmd);
    if (nsfReplacementProc != objProc) {
      ti->proc = objProc;
      ti->clientData = Tcl_Command_objClientData(cmd);
      if (nsfReplacementProc != NULL) {
	Tcl_Command_objProc(cmd) = nsfReplacementProc;
      }
      if (cd != NULL) {
        Tcl_Command_objClientData(cmd) = cd;
      }
    }
  }
  return result;
}

/*
 *----------------------------------------------------------------------
 * Nsf_InfoBodyObjCmd --
 *
 *    A TclObjCmd for shadowing "::tcl::info::body.  If called with an
 *    nsf::proc that is technically a command rather than a proc, the original
 *    command fails with  "not a proc".  If the body is from an nsf::proc, call
 *    tcl::info::body with the shadowed body.
 *
 *    Example:
 *       nsf::proc foo {-a} {puts $a};  info body foo
 *
 * Results:
 *    A Tcl return code.
 *
 * Side effects:
 *    None.
 *
 *----------------------------------------------------------------------
 */

static int
Nsf_InfoBodyObjCmd(ClientData UNUSED(clientData), Tcl_Interp *interp, int objc, Tcl_Obj *const objv[]) {
  Tcl_Command cmd;

  nonnull_assert(interp != NULL);
  nonnull_assert(objv != NULL);

  if (objc != 2) {
    /* wrong # args, let Tcl generate the error */
    return NsfCallCommand(interp, NSF_INFO_BODY, objc, objv);
  }

  cmd = Tcl_FindCommand(interp, ObjStr(objv[1]), (Tcl_Namespace *)NULL, 0);
  if (cmd != NULL) {
    Tcl_ObjCmdProc *proc =  Tcl_Command_objProc(cmd);
    ClientData      procClientData = Tcl_Command_objClientData(cmd);
    if (proc == NsfProcStub && procClientData != NULL) {
      NsfProcClientData *tcd = procClientData;
      Tcl_Obj *ov[2];
      /*
       * The command is from an nsf::proc
       */
      ov[0] = objv[0];
      ov[1] = tcd->procName;
      return NsfCallCommand(interp, NSF_INFO_BODY, objc, ov);
    }
  }

  /* Call the original "info body" */
  return NsfCallCommand(interp, NSF_INFO_BODY, objc, objv);
}


/*
 *----------------------------------------------------------------------
 * Nsf_RenameObjCmd --
 *
 *    TclObjCmd that wraps "::rename".  Destroys and/or renames the command if
 *    it refers to an NsfObject.  Otherwise, simply calls the original
 *    "::rename".
 *
 * Results:
 *    A Tcl return code.
 *
 * Side effects:
 *    None.
 *
 *----------------------------------------------------------------------
 */

static int
Nsf_RenameObjCmd(ClientData UNUSED(clientData), Tcl_Interp *interp, int objc, Tcl_Obj *const objv[]) {
  Tcl_Command cmd;

  if (objc != 3) {
    /* wrong # args, let Tcl generate the error */
    return NsfCallCommand(interp, NSF_RENAME, objc, objv);
  }

  /* if an obj/cl should be renamed => call the Nsf move method */
  cmd = Tcl_FindCommand(interp, ObjStr(objv[1]), (Tcl_Namespace *)NULL, 0);
  if (cmd != NULL) {
    Tcl_ObjCmdProc *proc =  Tcl_Command_objProc(cmd);
    ClientData      procClientData = Tcl_Command_objClientData(cmd);
    NsfObject      *object = NsfGetObjectFromCmdPtr(cmd);
    Tcl_Command     parentCmd;
    char           *newName = ObjStr(objv[2]);
    
    if (proc == NsfProcStub && procClientData != NULL &&
        *newName != '\0') {
      Tcl_DString fqNewName;
      int result;
      NsfProcClientData *tcd = procClientData;

      Tcl_DStringInit(&fqNewName);
      Tcl_DStringAppend(&fqNewName, "::nsf::procs::", 14);
      Tcl_DStringAppend(&fqNewName, newName, -1);
      
      /* fprintf(stderr, "oldName %s newName %s\n", ObjStr(tcd->procName), Tcl_DStringValue(&fqNewName));*/
      result = TclRenameCommand(interp, ObjStr(tcd->procName), Tcl_DStringValue(&fqNewName));
      
      if (result == TCL_OK) {
        DECR_REF_COUNT2("procNameObj", tcd->procName);
        tcd->procName = Tcl_NewStringObj(Tcl_DStringValue(&fqNewName),
                                         Tcl_DStringLength(&fqNewName));
        INCR_REF_COUNT2("procNameObj", tcd->procName);
      }

      Tcl_DStringFree(&fqNewName);

      if (result != TCL_OK) {
        return TCL_ERROR;
      }
      
    } else if (object != NULL) {
      Tcl_Obj *methodObj = NsfMethodObj(object, NSF_o_move_idx);
      
      if (methodObj) {
        return NsfCallMethodWithArgs(interp, (Nsf_Object *)object,
                                     methodObj, objv[2], 1, 0,
                                     NSF_CSC_IMMEDIATE);
      }
    }

    parentCmd = Tcl_FindCommand(interp,  Tcl_Command_nsPtr(cmd)->fullName,
                                (Tcl_Namespace *)NULL, 0);
    if (parentCmd != NULL) {
      NsfObjectMethodEpochIncr("::rename");
    }
  }

  /* Actually rename the cmd using Tcl's rename*/
  return NsfCallCommand(interp, NSF_RENAME, objc, objv);
}

/*
 *----------------------------------------------------------------------
 * Nsf_InfoFrameObjCmd --
 *
 *    TclObjCmd that wraps "::tcl::info::frame". Calls the shadowed method, and
 *    if it returns OK and the frame is an NSF frame, removes from the result
 *    the misleading "proc" and adds "method", "class", "object" and
 *    "frametype".
 *
 * Results:
 *    Tcl return code.
 *
 * Side effects:
 *    None.
 *
 *----------------------------------------------------------------------
 */

static int
Nsf_InfoFrameObjCmd(ClientData UNUSED(clientData), Tcl_Interp *interp, int objc, Tcl_Obj *const objv[]) {
  int result;

  result = NsfCallCommand(interp, NSF_INFO_FRAME, objc, objv);

  if (result == TCL_OK && objc == 2) {
    int          level, topLevel;
    unsigned int frameFlags;
    CmdFrame    *framePtr = Tcl_Interp_cmdFramePtr(interp);
    CallFrame   *varFramePtr = Tcl_Interp_varFramePtr(interp);
    Tcl_Obj     *resultObj = Tcl_GetObjResult(interp);

    /* 
     * Level must be ok or the result would not be TCL_OK.
     */
    Tcl_GetIntFromObj(interp, objv[1], &level);

    /* todo: Is coroutine level handling needed here? */
    topLevel = (framePtr == NULL) ? 0 :  framePtr->level;

    if (level > 0) {
      level -= topLevel;
    }

    while (++level <= 0 && varFramePtr && framePtr) {
      framePtr = framePtr->nextPtr;
      varFramePtr = varFramePtr->callerPtr;
    }

    frameFlags = (varFramePtr != NULL) ? (unsigned int)Tcl_CallFrame_isProcCallFrame(varFramePtr) : 0u;
    /*fprintf(stderr, " ... frame %p varFramePtr %p frameFlags %.6x\n",
      framePtr, varFramePtr, frameFlags);
      Tcl85showStack(interp);*/
    if (frameFlags & (FRAME_IS_NSF_METHOD|FRAME_IS_NSF_CMETHOD)) {
      NsfCallStackContent *cscPtr =
        ((NsfCallStackContent *)Tcl_CallFrame_clientData(varFramePtr));
      const char *frameType;
      Tcl_Obj *listObj, **ov;
      int oc, i;

      listObj = Tcl_NewListObj(0, NULL);
      
      /* 
       * Remove "proc" element from list, if provided.
       */
      Tcl_ListObjGetElements(interp, resultObj, &oc, &ov);
      for (i=0; i<oc; i += 2) {
	if (!strcmp(ObjStr(ov[i]), "proc")) {
	  continue;
	}
	Tcl_ListObjAppendElement(interp, listObj, ov[i]);
	Tcl_ListObjAppendElement(interp, listObj, ov[i+1]);
      }

      Tcl_ListObjAppendElement(interp, listObj, Tcl_NewStringObj("object", 6));
      Tcl_ListObjAppendElement(interp, listObj, cscPtr->self->cmdName);
      Tcl_ListObjAppendElement(interp, listObj, Tcl_NewStringObj("class", 5));
      Tcl_ListObjAppendElement(interp, listObj, (cscPtr->cl != NULL) ? cscPtr->cl->object.cmdName
			       : NsfGlobalObjs[NSF_EMPTY]);
      Tcl_ListObjAppendElement(interp, listObj, Tcl_NewStringObj("method", 6));
      Tcl_ListObjAppendElement(interp, listObj,
                               (cscPtr->cmdPtr != NULL)
                               ? Tcl_NewStringObj(Tcl_GetCommandName(interp, cscPtr->cmdPtr), -1)
			       : NsfGlobalObjs[NSF_EMPTY]);
      Tcl_ListObjAppendElement(interp, listObj, Tcl_NewStringObj("frametype", 9));
      if (cscPtr->frameType == NSF_CSC_TYPE_PLAIN) {
        frameType = "intrinsic";
      } else if (cscPtr->frameType & NSF_CSC_TYPE_ACTIVE_MIXIN) {
        frameType = "mixin";
      } else if (cscPtr->frameType & NSF_CSC_TYPE_ACTIVE_FILTER) {
        frameType = "filter";
      } else if (cscPtr->frameType & NSF_CSC_TYPE_GUARD) {
        frameType = "guard";
      } else {
        frameType = "unknown";
      }
      Tcl_ListObjAppendElement(interp, listObj, Tcl_NewStringObj(frameType, -1));
      Tcl_SetObjResult(interp, listObj);
    } else if (frameFlags & (FRAME_IS_NSF_OBJECT)) {
      NsfObject *object = (NsfObject *)Tcl_CallFrame_clientData(varFramePtr);
      /* Tcl_Obj *listObj = Tcl_NewListObj(0, NULL); */

      Tcl_ListObjAppendElement(interp, resultObj, Tcl_NewStringObj("object", 6));
      Tcl_ListObjAppendElement(interp, resultObj, object->cmdName);
      Tcl_ListObjAppendElement(interp, resultObj, Tcl_NewStringObj("frameType", 9));
      Tcl_ListObjAppendElement(interp, resultObj, Tcl_NewStringObj("object", 6));
      Tcl_SetObjResult(interp, resultObj);
    }
  }

  return result;
}


/*
 *----------------------------------------------------------------------
 * NsfShadowTclCommands --
 *
 *    Loads, refreshes, or unloads overridden Tcl commands. This routine serves
 *    two purposes:
 *
 *        Looks up some Tcl ObjProcs which are not available via global
 *        symbols in order to pass them to NsfCallCommand().
 *
 *        Performs pre- and/or postprocessing on Tcl commands are actually
 *        overridden.
 *
 * Results:
 *    A Tcl return code.
 *
 * Side effects:
 *    None.
 *
 *----------------------------------------------------------------------
 */
#define CMD_INFO(rst, name) &(rst)->tclCommands[(name)-NSF_EXPR]

int
NsfShadowTclCommands(Tcl_Interp *interp, NsfShadowOperations load) {
  int              rc = TCL_OK;
  NsfRuntimeState *rst;

  nonnull_assert(interp != NULL);

  rst = RUNTIME_STATE(interp);
 
  if (load == SHADOW_LOAD) {

    assert(rst->tclCommands == NULL);
    rst->tclCommands = NEW_ARRAY(NsfShadowTclCommandInfo, NSF_RENAME - NSF_EXPR + 1);

#ifdef USE_TCL_STUBS
    /*
     * When the third argument of NsfReplaceCommand is NULL the commands are
     * not overloaded, but this mechanism is still used to call Tcl commands
     * like Tcl_ExprObjCmd(), Tcl_IncrObjCmd() and Tcl_SubstObjCmd(), which are
     * not available in the stub table.
     */
    rc |= NsfReplaceCommand(interp, NsfGlobalObjs[NSF_EXPR],       NULL, NULL, CMD_INFO(rst, NSF_EXPR));
#endif
    rc |= NsfReplaceCommand(interp, NsfGlobalObjs[NSF_FORMAT],     NULL, NULL, CMD_INFO(rst, NSF_FORMAT));
    rc |= NsfReplaceCommand(interp, NsfGlobalObjs[NSF_INTERP],     NULL, NULL, CMD_INFO(rst, NSF_INTERP));
    rc |= NsfReplaceCommand(interp, NsfGlobalObjs[NSF_STRING_IS],  NULL, NULL, CMD_INFO(rst, NSF_STRING_IS));
    rc |= NsfReplaceCommand(interp, NsfGlobalObjs[NSF_DISASSEMBLE],  NULL, NULL, CMD_INFO(rst, NSF_DISASSEMBLE));

    /* add custom semantics to the following commands */
    rc |= NsfReplaceCommand(interp, NsfGlobalObjs[NSF_INFO_BODY],  Nsf_InfoBodyObjCmd,  NULL, CMD_INFO(rst, NSF_INFO_BODY));
    rc |= NsfReplaceCommand(interp, NsfGlobalObjs[NSF_INFO_FRAME], Nsf_InfoFrameObjCmd, NULL, CMD_INFO(rst, NSF_INFO_FRAME));
    rc |= NsfReplaceCommand(interp, NsfGlobalObjs[NSF_RENAME],     Nsf_RenameObjCmd,    NULL, CMD_INFO(rst, NSF_RENAME));

  } else if (load == SHADOW_REFETCH) {
    NsfReplaceCommandCheck(interp, NsfGlobalObjs[NSF_INFO_BODY],   Nsf_InfoFrameObjCmd, CMD_INFO(rst, NSF_INFO_BODY));
    NsfReplaceCommandCheck(interp, NsfGlobalObjs[NSF_INFO_FRAME],  Nsf_InfoFrameObjCmd, CMD_INFO(rst, NSF_INFO_FRAME));
    NsfReplaceCommandCheck(interp, NsfGlobalObjs[NSF_RENAME],      Nsf_RenameObjCmd,    CMD_INFO(rst, NSF_RENAME));
  } else {
    NsfReplaceCommandCleanup(interp, NsfGlobalObjs[NSF_INFO_BODY],  CMD_INFO(rst, NSF_INFO_BODY));
    NsfReplaceCommandCleanup(interp, NsfGlobalObjs[NSF_INFO_FRAME], CMD_INFO(rst, NSF_INFO_FRAME));
    NsfReplaceCommandCleanup(interp, NsfGlobalObjs[NSF_RENAME],     CMD_INFO(rst, NSF_RENAME));

    FREE(NsfShadowTclCommandInfo*, rst->tclCommands);
    rst->tclCommands = NULL;
  }

  return rc;
}

/*
 *----------------------------------------------------------------------
 * NsfCallCommand --
 *
 *    Looks up and calls a Tcl command in the table previously populated by
 *    NsfShadowTclCommands(), replacing objv[0] with the previously-stored name
 *    of the command.
 *
 * Results:
 *    A Tcl return code.
 *
 * Side effects:
 *    None.
 *
 *----------------------------------------------------------------------
 */
int NsfCallCommand(Tcl_Interp *interp, NsfGlobalNames name,
	    int objc, Tcl_Obj *const objv[]) {
  int result;
  NsfShadowTclCommandInfo *ti = &RUNTIME_STATE(interp)->tclCommands[name-NSF_EXPR];
  ALLOC_ON_STACK(Tcl_Obj*, objc, ov);
  /*
   {int i;
    fprintf(stderr, "calling %s (%p %p) in %p, objc=%d ",
	    NsfGlobalStrings[name], ti, ti->proc, interp, objc);
            for(i=0;i<objc;i++){fprintf(stderr, "'%s' ", ObjStr(objv[i]));}
    fprintf(stderr, "\n");
  }
  */
  ov[0] = NsfGlobalObjs[name];
  if (objc > 1) {
    memcpy(ov+1, objv+1, sizeof(Tcl_Obj *) * ((size_t)objc - 1u));
  }
  result = Tcl_NRCallObjProc(interp, ti->proc, ti->clientData, objc, objv);
  FREE_ON_STACK(Tcl_Obj *, ov);
  return result;
}

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 2
 * fill-column: 78
 * indent-tabs-mode: nil
 * End:
 */
