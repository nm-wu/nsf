/*
 * nsfDecls.h --
 *
 *	Declarations of functions in the platform independent public XOTcl API.
 *
 *  Copyright (C) 1999-2008 Gustaf Neumann, Uwe Zdun
 *
 * See the file "tcl-license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#ifndef _XOTCLDECLS
#define _XOTCLDECLS

/*
 * WARNING: This file is automatically generated by the tools/genStubs.tcl
 * script.  Any modifications to the function declarations below should be made
 * in the nsf.decls script.
 */

/* !BEGIN!: Do not edit below this line. */

/*
 * Exported function declarations:
 */

#ifndef Nsf_Init_TCL_DECLARED
#define Nsf_Init_TCL_DECLARED
/* 0 */
EXTERN int		Nsf_Init(Tcl_Interp *interp);
#endif
/* Slot 1 is reserved */
#ifndef NsfIsClass_TCL_DECLARED
#define NsfIsClass_TCL_DECLARED
/* 2 */
EXTERN struct Nsf_Class * NsfIsClass(Tcl_Interp *interp, ClientData cd);
#endif
#ifndef NsfGetObject_TCL_DECLARED
#define NsfGetObject_TCL_DECLARED
/* 3 */
EXTERN struct Nsf_Object * NsfGetObject(Tcl_Interp *interp, CONST char *name);
#endif
#ifndef NsfGetClass_TCL_DECLARED
#define NsfGetClass_TCL_DECLARED
/* 4 */
EXTERN struct Nsf_Class * NsfGetClass(Tcl_Interp *interp, CONST char *name);
#endif
#ifndef NsfDeleteObject_TCL_DECLARED
#define NsfDeleteObject_TCL_DECLARED
/* 5 */
EXTERN int		NsfDeleteObject(Tcl_Interp *interp,
				struct Nsf_Object *object);
#endif
#ifndef NsfRemoveObjectMethod_TCL_DECLARED
#define NsfRemoveObjectMethod_TCL_DECLARED
/* 6 */
EXTERN int		NsfRemoveObjectMethod(Tcl_Interp *interp,
				struct Nsf_Object *object, CONST char *nm);
#endif
#ifndef NsfRemoveClassMethod_TCL_DECLARED
#define NsfRemoveClassMethod_TCL_DECLARED
/* 7 */
EXTERN int		NsfRemoveClassMethod(Tcl_Interp *interp,
				struct Nsf_Class *cl, CONST char *nm);
#endif
#ifndef Nsf_ObjSetVar2_TCL_DECLARED
#define Nsf_ObjSetVar2_TCL_DECLARED
/* 8 */
EXTERN Tcl_Obj *	Nsf_ObjSetVar2(struct Nsf_Object *object,
				Tcl_Interp *interp, Tcl_Obj *name1,
				Tcl_Obj *name2, Tcl_Obj *value, int flgs);
#endif
#ifndef Nsf_ObjGetVar2_TCL_DECLARED
#define Nsf_ObjGetVar2_TCL_DECLARED
/* 9 */
EXTERN Tcl_Obj *	Nsf_ObjGetVar2(struct Nsf_Object *object,
				Tcl_Interp *interp, Tcl_Obj *name1,
				Tcl_Obj *name2, int flgs);
#endif
#ifndef Nsf_UnsetVar2_TCL_DECLARED
#define Nsf_UnsetVar2_TCL_DECLARED
/* 10 */
EXTERN int		Nsf_UnsetVar2(struct Nsf_Object *object,
				Tcl_Interp *interp, CONST char *name1,
				CONST char *name2, int flgs);
#endif
#ifndef NsfDStringPrintf_TCL_DECLARED
#define NsfDStringPrintf_TCL_DECLARED
/* 11 */
EXTERN void		NsfDStringPrintf(Tcl_DString *dsPtr, CONST char *fmt,
				va_list apSrc);
#endif
#ifndef NsfPrintError_TCL_DECLARED
#define NsfPrintError_TCL_DECLARED
/* 12 */
EXTERN int		NsfPrintError(Tcl_Interp *interp, CONST char *fmt, ...);
#endif
#ifndef NsfErrInProc_TCL_DECLARED
#define NsfErrInProc_TCL_DECLARED
/* 13 */
EXTERN int		NsfErrInProc(Tcl_Interp *interp, Tcl_Obj *objName,
				Tcl_Obj *clName, CONST char *procName);
#endif
#ifndef NsfObjErrType_TCL_DECLARED
#define NsfObjErrType_TCL_DECLARED
/* 14 */
EXTERN int		NsfObjErrType(Tcl_Interp *interp,
				CONST char *context, Tcl_Obj *value,
				CONST char *type, Nsf_Param CONST *pPtr);
#endif
#ifndef NsfStackDump_TCL_DECLARED
#define NsfStackDump_TCL_DECLARED
/* 15 */
EXTERN void		NsfStackDump(Tcl_Interp *interp);
#endif
#ifndef NsfSetObjClientData_TCL_DECLARED
#define NsfSetObjClientData_TCL_DECLARED
/* 16 */
EXTERN void		NsfSetObjClientData(Tcl_Interp *interp,
				Nsf_Object *object, ClientData data);
#endif
#ifndef NsfGetObjClientData_TCL_DECLARED
#define NsfGetObjClientData_TCL_DECLARED
/* 17 */
EXTERN ClientData	NsfGetObjClientData(Tcl_Interp *interp,
				Nsf_Object *object);
#endif
#ifndef NsfSetClassClientData_TCL_DECLARED
#define NsfSetClassClientData_TCL_DECLARED
/* 18 */
EXTERN void		NsfSetClassClientData(Tcl_Interp *interp,
				Nsf_Class *cl, ClientData data);
#endif
#ifndef NsfGetClassClientData_TCL_DECLARED
#define NsfGetClassClientData_TCL_DECLARED
/* 19 */
EXTERN ClientData	NsfGetClassClientData(Tcl_Interp *interp,
				Nsf_Class *cl);
#endif
#ifndef NsfRequireObjNamespace_TCL_DECLARED
#define NsfRequireObjNamespace_TCL_DECLARED
/* 20 */
EXTERN void		NsfRequireObjNamespace(Tcl_Interp *interp,
				Nsf_Object *object);
#endif
#ifndef NsfCallMethodWithArgs_TCL_DECLARED
#define NsfCallMethodWithArgs_TCL_DECLARED
/* 21 */
EXTERN int		NsfCallMethodWithArgs(Tcl_Interp *interp,
				Nsf_Object *object, Tcl_Obj *method,
				Tcl_Obj *arg, int objc,
				Tcl_Obj *CONST objv[], int flags);
#endif
#ifndef NsfAddObjectMethod_TCL_DECLARED
#define NsfAddObjectMethod_TCL_DECLARED
/* 22 */
EXTERN int		NsfAddObjectMethod(Tcl_Interp *interp,
				struct Nsf_Object *object, CONST char *nm,
				Tcl_ObjCmdProc *proc, ClientData cd,
				Tcl_CmdDeleteProc *dp, int flags);
#endif
#ifndef NsfAddClassMethod_TCL_DECLARED
#define NsfAddClassMethod_TCL_DECLARED
/* 23 */
EXTERN int		NsfAddClassMethod(Tcl_Interp *interp,
				struct Nsf_Class *cl, CONST char *nm,
				Tcl_ObjCmdProc *proc, ClientData cd,
				Tcl_CmdDeleteProc *dp, int flags);
#endif
#ifndef NsfCreate_TCL_DECLARED
#define NsfCreate_TCL_DECLARED
/* 24 */
EXTERN int		NsfCreate(Tcl_Interp *in, Nsf_Class *class,
				Tcl_Obj *name, int objc,
				Tcl_Obj *CONST objv[]);
#endif

typedef struct NsfStubHooks {
    struct NsfIntStubs *nsfIntStubs;
} NsfStubHooks;

typedef struct NsfStubs {
    int magic;
    struct NsfStubHooks *hooks;

    int (*nsf_Init) (Tcl_Interp *interp); /* 0 */
    VOID *reserved1;
    struct Nsf_Class * (*nsfIsClass) (Tcl_Interp *interp, ClientData cd); /* 2 */
    struct Nsf_Object * (*nsfGetObject) (Tcl_Interp *interp, CONST char *name); /* 3 */
    struct Nsf_Class * (*nsfGetClass) (Tcl_Interp *interp, CONST char *name); /* 4 */
    int (*nsfDeleteObject) (Tcl_Interp *interp, struct Nsf_Object *object); /* 5 */
    int (*nsfRemoveObjectMethod) (Tcl_Interp *interp, struct Nsf_Object *object, CONST char *nm); /* 6 */
    int (*nsfRemoveClassMethod) (Tcl_Interp *interp, struct Nsf_Class *cl, CONST char *nm); /* 7 */
    Tcl_Obj * (*nsf_ObjSetVar2) (struct Nsf_Object *object, Tcl_Interp *interp, Tcl_Obj *name1, Tcl_Obj *name2, Tcl_Obj *value, int flgs); /* 8 */
    Tcl_Obj * (*nsf_ObjGetVar2) (struct Nsf_Object *object, Tcl_Interp *interp, Tcl_Obj *name1, Tcl_Obj *name2, int flgs); /* 9 */
    int (*nsf_UnsetVar2) (struct Nsf_Object *object, Tcl_Interp *interp, CONST char *name1, CONST char *name2, int flgs); /* 10 */
    void (*nsfDStringPrintf) (Tcl_DString *dsPtr, CONST char *fmt, va_list apSrc); /* 11 */
    int (*nsfPrintError) (Tcl_Interp *interp, CONST char *fmt, ...); /* 12 */
    int (*nsfErrInProc) (Tcl_Interp *interp, Tcl_Obj *objName, Tcl_Obj *clName, CONST char *procName); /* 13 */
    int (*nsfObjErrType) (Tcl_Interp *interp, CONST char *context, Tcl_Obj *value, CONST char *type, Nsf_Param CONST *pPtr); /* 14 */
    void (*nsfStackDump) (Tcl_Interp *interp); /* 15 */
    void (*nsfSetObjClientData) (Tcl_Interp *interp, Nsf_Object *object, ClientData data); /* 16 */
    ClientData (*nsfGetObjClientData) (Tcl_Interp *interp, Nsf_Object *object); /* 17 */
    void (*nsfSetClassClientData) (Tcl_Interp *interp, Nsf_Class *cl, ClientData data); /* 18 */
    ClientData (*nsfGetClassClientData) (Tcl_Interp *interp, Nsf_Class *cl); /* 19 */
    void (*nsfRequireObjNamespace) (Tcl_Interp *interp, Nsf_Object *object); /* 20 */
    int (*nsfCallMethodWithArgs) (Tcl_Interp *interp, Nsf_Object *object, Tcl_Obj *method, Tcl_Obj *arg, int objc, Tcl_Obj *CONST objv[], int flags); /* 21 */
    int (*nsfAddObjectMethod) (Tcl_Interp *interp, struct Nsf_Object *object, CONST char *nm, Tcl_ObjCmdProc *proc, ClientData cd, Tcl_CmdDeleteProc *dp, int flags); /* 22 */
    int (*nsfAddClassMethod) (Tcl_Interp *interp, struct Nsf_Class *cl, CONST char *nm, Tcl_ObjCmdProc *proc, ClientData cd, Tcl_CmdDeleteProc *dp, int flags); /* 23 */
    int (*nsfCreate) (Tcl_Interp *in, Nsf_Class *class, Tcl_Obj *name, int objc, Tcl_Obj *CONST objv[]); /* 24 */
} NsfStubs;

#ifdef __cplusplus
extern "C" {
#endif
extern NsfStubs *nsfStubsPtr;
#ifdef __cplusplus
}
#endif

#if defined(USE_NSF_STUBS) && !defined(USE_NSF_STUB_PROCS)

/*
 * Inline function declarations:
 */

#ifndef Nsf_Init
#define Nsf_Init \
	(nsfStubsPtr->nsf_Init) /* 0 */
#endif
/* Slot 1 is reserved */
#ifndef NsfIsClass
#define NsfIsClass \
	(nsfStubsPtr->nsfIsClass) /* 2 */
#endif
#ifndef NsfGetObject
#define NsfGetObject \
	(nsfStubsPtr->nsfGetObject) /* 3 */
#endif
#ifndef NsfGetClass
#define NsfGetClass \
	(nsfStubsPtr->nsfGetClass) /* 4 */
#endif
#ifndef NsfDeleteObject
#define NsfDeleteObject \
	(nsfStubsPtr->nsfDeleteObject) /* 5 */
#endif
#ifndef NsfRemoveObjectMethod
#define NsfRemoveObjectMethod \
	(nsfStubsPtr->nsfRemoveObjectMethod) /* 6 */
#endif
#ifndef NsfRemoveClassMethod
#define NsfRemoveClassMethod \
	(nsfStubsPtr->nsfRemoveClassMethod) /* 7 */
#endif
#ifndef Nsf_ObjSetVar2
#define Nsf_ObjSetVar2 \
	(nsfStubsPtr->nsf_ObjSetVar2) /* 8 */
#endif
#ifndef Nsf_ObjGetVar2
#define Nsf_ObjGetVar2 \
	(nsfStubsPtr->nsf_ObjGetVar2) /* 9 */
#endif
#ifndef Nsf_UnsetVar2
#define Nsf_UnsetVar2 \
	(nsfStubsPtr->nsf_UnsetVar2) /* 10 */
#endif
#ifndef NsfDStringPrintf
#define NsfDStringPrintf \
	(nsfStubsPtr->nsfDStringPrintf) /* 11 */
#endif
#ifndef NsfPrintError
#define NsfPrintError \
	(nsfStubsPtr->nsfPrintError) /* 12 */
#endif
#ifndef NsfErrInProc
#define NsfErrInProc \
	(nsfStubsPtr->nsfErrInProc) /* 13 */
#endif
#ifndef NsfObjErrType
#define NsfObjErrType \
	(nsfStubsPtr->nsfObjErrType) /* 14 */
#endif
#ifndef NsfStackDump
#define NsfStackDump \
	(nsfStubsPtr->nsfStackDump) /* 15 */
#endif
#ifndef NsfSetObjClientData
#define NsfSetObjClientData \
	(nsfStubsPtr->nsfSetObjClientData) /* 16 */
#endif
#ifndef NsfGetObjClientData
#define NsfGetObjClientData \
	(nsfStubsPtr->nsfGetObjClientData) /* 17 */
#endif
#ifndef NsfSetClassClientData
#define NsfSetClassClientData \
	(nsfStubsPtr->nsfSetClassClientData) /* 18 */
#endif
#ifndef NsfGetClassClientData
#define NsfGetClassClientData \
	(nsfStubsPtr->nsfGetClassClientData) /* 19 */
#endif
#ifndef NsfRequireObjNamespace
#define NsfRequireObjNamespace \
	(nsfStubsPtr->nsfRequireObjNamespace) /* 20 */
#endif
#ifndef NsfCallMethodWithArgs
#define NsfCallMethodWithArgs \
	(nsfStubsPtr->nsfCallMethodWithArgs) /* 21 */
#endif
#ifndef NsfAddObjectMethod
#define NsfAddObjectMethod \
	(nsfStubsPtr->nsfAddObjectMethod) /* 22 */
#endif
#ifndef NsfAddClassMethod
#define NsfAddClassMethod \
	(nsfStubsPtr->nsfAddClassMethod) /* 23 */
#endif
#ifndef NsfCreate
#define NsfCreate \
	(nsfStubsPtr->nsfCreate) /* 24 */
#endif

#endif /* defined(USE_NSF_STUBS) && !defined(USE_NSF_STUB_PROCS) */

/* !END!: Do not edit above this line. */

#endif /* _XOTCLDECLS */

