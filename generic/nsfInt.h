/* 
 *  nsfInt.h --
 *  
 *      Declarations of the internally used API Functions of the Next
 *      Scripting Framework.
 *
 *  Copyright (C) 1999-2013 Gustaf Neumann
 *  Copyright (C) 1999-2007 Uwe Zdun
 *  Copyright (C) 2011 Stefan Sobernig
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
 */

#ifndef _nsf_int_h_
#define _nsf_int_h_

#if defined(HAVE_STDINT_H)
# define HAVE_INTPTR_T 
# define HAVE_UINTPTR_T 
#endif

/*
 * MinGW and MinGW-w64 provide both MSCRT-compliant and ANSI-compliant
 * implementations of certain I/O operations (e.g., *printf()). By
 * setting __USE_MINGW_ANSI_STDIO to 1 explicitly, we can assume the
 * ANSI versions.
 *
 * Note: It is sufficient to test for __MINGW32__ to trap all MinGW
 * tool chains, including 64bit versions. See
 * http://sourceforge.net/p/predef/wiki/Compilers/#mingw-and-mingw-w64
 */

#if defined(__MINGW32__) && !defined(__USE_MINGW_ANSI_STDIO)
#define __USE_MINGW_ANSI_STDIO 1
#endif

#include <tclInt.h>
#include "nsf.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#if defined(HAVE_TCL_COMPILE_H)
# include <tclCompile.h>
#endif

#if defined(NSF_PROFILE)
# include <sys/time.h>
#endif

#if defined(__GNUC__) && __GNUC__ > 2
/* Use gcc branch prediction hint to minimize cost of e.g. DTrace
 * ENABLED checks. 
 */
#  define unlikely(x) (__builtin_expect((x), 0))
#  define likely(x) (__builtin_expect((x), 1))
#else
#  define unlikely(x) (x)
#  define likely(x) (x)
#endif

#if defined(NSF_DTRACE)
# include "nsfDTrace.h"
# define NSF_DTRACE_METHOD_ENTRY_ENABLED()     		unlikely(NSF_METHOD_ENTRY_ENABLED())
# define NSF_DTRACE_METHOD_RETURN_ENABLED()    		unlikely(NSF_METHOD_RETURN_ENABLED())
# define NSF_DTRACE_OBJECT_ALLOC_ENABLED()		unlikely(NSF_OBJECT_ALLOC_ENABLED())
# define NSF_DTRACE_OBJECT_FREE_ENABLED()  		unlikely(NSF_OBJECT_FREE_ENABLED())
# define NSF_DTRACE_CONFIGURE_PROBE_ENABLED()  		unlikely(NSF_CONFIGURE_PROBE_ENABLED())
# define NSF_DTRACE_METHOD_ENTRY(a0, a1, a2, a3, a4)	NSF_METHOD_ENTRY(a0, a1, a2, a3, a4)
# define NSF_DTRACE_METHOD_RETURN(a0, a1, a2, a3)      	NSF_METHOD_RETURN(a0, a1, a2, a3)
# define NSF_DTRACE_OBJECT_ALLOC(a0, a1)		NSF_OBJECT_ALLOC(a0, a1)
# define NSF_DTRACE_OBJECT_FREE(a0, a1)			NSF_OBJECT_FREE(a0, a1)
# define NSF_DTRACE_CONFIGURE_PROBE(a0, a1)      	NSF_CONFIGURE_PROBE(a0, a1)
#else
# define NSF_DTRACE_METHOD_ENTRY_ENABLED()     		0
# define NSF_DTRACE_METHOD_RETURN_ENABLED()    		0
# define NSF_DTRACE_OBJECT_ALLOC_ENABLED()		0
# define NSF_DTRACE_OBJECT_FREE_ENABLED()  		0
# define NSF_DTRACE_CONFIGURE_PROBE_ENABLED()		0
# define NSF_DTRACE_METHOD_ENTRY(a0, a1, a2, a3, a4)   	{}
# define NSF_DTRACE_METHOD_RETURN(a0, a1, a2, a3)      	{}
# define NSF_DTRACE_OBJECT_ALLOC(a0, a1)		{}
# define NSF_DTRACE_OBJECT_FREE(a0, a1)			{}
# define NSF_DTRACE_CONFIGURE_PROBE(a0, a1)      	{}
#endif


#ifdef DMALLOC
#  include "dmalloc.h"
#endif

/*
 * Makros
 */

#if defined(PRE86)
# define Tcl_NRCallObjProc(interp, proc, cd, objc, objv) \
  (*(proc))((cd), (interp), (objc), (objv))
#endif

#ifdef NSF_MEM_COUNT
EXTERN int nsfMemCountInterpCounter;
typedef struct NsfMemCounter {
  int peak;
  int count;
} NsfMemCounter;
#  define MEM_COUNT_ALLOC(id,p) NsfMemCountAlloc(id, p)
#  define MEM_COUNT_FREE(id,p) NsfMemCountFree(id, p)
#  define MEM_COUNT_INIT() NsfMemCountInit()
#  define MEM_COUNT_RELEASE() NsfMemCountRelease()
#else
#  define MEM_COUNT_ALLOC(id,p)
#  define MEM_COUNT_FREE(id,p)
#  define MEM_COUNT_INIT()
#  define MEM_COUNT_RELEASE()
#endif

# define STRING_NEW(target, p, l)  target = ckalloc(l+1); strncpy(target, p, l); *((target)+l) = '\0'; \
  MEM_COUNT_ALLOC(#target, target)
# define STRING_FREE(key, p)  MEM_COUNT_FREE(key, p); ckfree((p))

/*
 * Tries to use gcc __attribute__ unused and mangles the name, so the
 * attribute could not be used, if declared as unused.
 */
#ifdef UNUSED
#elif defined(__GNUC__)
# define UNUSED(x) UNUSED_ ## x __attribute__((unused))
#elif defined(__LCLINT__)
# define UNUSED(x) /*@unused@*/ x
#else
# define UNUSED(x) x
#endif

#define DSTRING_INIT(dsPtr) Tcl_DStringInit(dsPtr); MEM_COUNT_ALLOC("DString",dsPtr)
#define DSTRING_FREE(dsPtr) \
  if ((dsPtr)->string != (dsPtr)->staticSpace) {Tcl_DStringFree(dsPtr);} MEM_COUNT_FREE("DString",dsPtr)

#if USE_ASSOC_DATA
# define RUNTIME_STATE(interp) ((NsfRuntimeState*)Tcl_GetAssocData((interp), "NsfRuntimeState", NULL))
#else
# define RUNTIME_STATE(interp) ((NsfRuntimeState*)((Interp*)(interp))->globalNsPtr->clientData)
#endif

#define nr_elements(arr)  ((int) (sizeof(arr) / sizeof(arr[0])))

# define NEW(type) \
	(type *)ckalloc(sizeof(type)); MEM_COUNT_ALLOC(#type, NULL)
# define NEW_ARRAY(type,n) \
	(type *)ckalloc(sizeof(type)*(n)); MEM_COUNT_ALLOC(#type "*", NULL)
# define FREE(type, var) \
	ckfree((char*) var); MEM_COUNT_FREE(#type,var)

#define isAbsolutePath(m) (*m == ':' && m[1] == ':')
#define isArgsString(m) (\
	*m   == 'a' && m[1] == 'r' && m[2] == 'g' && m[3] == 's' && \
	m[4] == '\0')
#define isBodyString(m) (\
	*m   == 'b' && m[1] == 'o' && m[2] == 'd' && m[3] == 'y' && \
	m[4] == '\0')
#define isCheckString(m) (\
	*m   == 'c' && m[1] == 'h' && m[2] == 'e' && m[3] == 'c' && \
	m[4] == 'k' && m[5] == '\0')
#define isCheckObjString(m) (\
        *m   == 'c' && m[1] == 'h' && m[2] == 'e' && m[3] == 'c' && \
	m[4] == 'k' && m[5] == 'o' && m[6] == 'b' && m[7] == 'j' && \
	m[8] == '\0')
#define isCreateString(m) (\
	*m   == 'c' && m[1] == 'r' && m[2] == 'e' && m[3] == 'a' && \
	m[4] == 't' && m[5] == 'e' && m[6] == '\0')
#define isTypeString(m) (\
	*m   == 't' && m[1] == 'y' && m[2] == 'p' && m[3] == 'e' && \
	m[4] == '\0')
#define isObjectString(m) (\
	*m   == 'o' && m[1] == 'b' && m[2] == 'j' && m[3] == 'e' && \
	m[4] == 'c' && m[5] == 't' && m[6] == '\0')
#define isClassString(m) (\
	*m   == 'c' && m[1] == 'l' && m[2] == 'a' && m[3] == 's' && \
	m[4] == 's' && m[5] == '\0')

#if (defined(sun) || defined(__hpux)) && !defined(__GNUC__)
#  define USE_ALLOCA
#endif

#if defined(__IBMC__) && !defined(__GNUC__)
# if __IBMC__ >= 0x0306
#  define USE_ALLOCA
# else
#  define USE_MALLOC
# endif
#endif

#if defined(VISUAL_CC)
#  define USE_MALLOC
#endif

#if defined(__GNUC__) && !defined(USE_ALLOCA) && !defined(USE_MALLOC)
# if !defined(NDEBUG)
#  define ALLOC_ON_STACK(type,n,var) \
    int __##var##_count = (n); type __##var[n+2]; \
    type *var = __##var + 1; var[-1] = var[__##var##_count] = (type)0xdeadbeaf
#  define FREE_ON_STACK(type,var)                                       \
  assert(var[-1] == var[__##var##_count] && var[-1] == (type)0xdeadbeaf)
# else
#  define ALLOC_ON_STACK(type,n,var) type var[(n)]
#  define FREE_ON_STACK(type,var)
# endif
#elif defined(USE_ALLOCA)
#  define ALLOC_ON_STACK(type,n,var) type *var = (type *)alloca((n)*sizeof(type))
#  define FREE_ON_STACK(type,var)
#else
#  define ALLOC_ON_STACK(type,n,var) type *var = (type *)ckalloc((n)*sizeof(type))
#  define FREE_ON_STACK(type,var) ckfree((char*)var)
#endif

#ifdef USE_ALLOCA
# include <alloca.h>
#endif

/*
 * This was defined to be inline for anything !sun or __IBMC__ >= 0x0306,
 * but __hpux should also be checked - switched to only allow in gcc - JH
 */
#if defined(__GNUC__) && !defined(__STRICT_ANSI__)
# define NSF_INLINE inline
#else
# define NSF_INLINE
#endif

#ifdef USE_TCL_STUBS
# define DECR_REF_COUNT(A) \
	MEM_COUNT_FREE("INCR_REF_COUNT" #A,A); assert((A)->refCount > -1); \
        Tcl_DecrRefCount(A)
# define DECR_REF_COUNT2(name,A)					\
	MEM_COUNT_FREE("INCR_REF_COUNT-" name,A); assert((A)->refCount > -1); \
        Tcl_DecrRefCount(A)
#else
# define DECR_REF_COUNT(A) \
	MEM_COUNT_FREE("INCR_REF_COUNT" #A,A); TclDecrRefCount(A)
# define DECR_REF_COUNT2(name,A)				\
	MEM_COUNT_FREE("INCR_REF_COUNT-" name,A); TclDecrRefCount(A)
#endif

#define INCR_REF_COUNT(A) MEM_COUNT_ALLOC("INCR_REF_COUNT"#A,A); Tcl_IncrRefCount(A)
#define INCR_REF_COUNT2(name,A) \
  /*fprintf(stderr, "c '%s'\n", ObjStr(A));*/				\
  MEM_COUNT_ALLOC("INCR_REF_COUNT-" name,A); Tcl_IncrRefCount(A)

#define ObjStr(obj) (obj)->bytes ? (obj)->bytes : Tcl_GetString(obj)
#define ClassName(cl) (((cl) ? ObjStr((cl)->object.cmdName) : "NULL"))
#define ObjectName(obj) (((obj) ? ObjStr((obj)->cmdName) : "NULL"))

#ifdef OBJDELETION_TRACE
# define PRINTOBJ(ctx,obj) \
  fprintf(stderr, "  %s %p %s oid=%p teardown=%p destroyCalled=%d\n", \
          ctx,obj,(obj)->teardown?ObjStr((obj)->cmdName):"(deleted)", \
          (obj)->id, (obj)->teardown,                                 \
          ((obj)->flags & NSF_DESTROY_CALLED))
#else
# define PRINTOBJ(ctx,obj)
#endif

/*
 * When an integer is printed, it might take so many digits
 */
#define LONG_AS_STRING 32

/* TCL_CONTINUE is defined as 4, from 5 on we can
   use app-specific return codes */
#define NSF_CHECK_FAILED 6


/*
  The NsfConfigEnabled() macro allows for querying whether a
  configuration macro (see above) is actually defined (and whether it
  expands to 1). This macro can be used both in CPP expressions (e.g.,
  "#if NsfConfigEnabled(...)") and in C expressions (e.g.,
  "if(NsfConfigEnabled(...))")

  Adapted from https://plus.google.com/+LinusTorvalds/posts/9gntjh57dXt
*/

#define NsfConfigEnabled(macro) NsfConfigEnabled_(macro)
#define NsfMacroTest_1 ,
#define NsfConfigEnabled_(value) NsfConfigEnabled__(NsfMacroTest_##value)
#define NsfConfigEnabled__(comma) NsfConfigEnabled___(comma 1, 0)
#define NsfConfigEnabled___(_, v, ...) v

/*
 *
 * Next Scripting Structures
 *
 */

/*
 * Filter structures
 */
typedef struct NsfFilterStack {
  Tcl_Command currentCmdPtr;
  Tcl_Obj *calledProc;
  struct NsfFilterStack *nextPtr;
} NsfFilterStack;


/*
 * Assertion structures
 */

typedef struct NsfTclObjList {
  Tcl_Obj *content;
  Tcl_Obj *payload;
  struct NsfTclObjList *nextPtr;
} NsfTclObjList;

typedef struct NsfProcAssertion {
  NsfTclObjList *pre;
  NsfTclObjList *post;
} NsfProcAssertion;

typedef struct NsfAssertionStore {
  NsfTclObjList *invariants;
  Tcl_HashTable procs;
} NsfAssertionStore;

typedef enum { /* powers of 2; add to ALL, if default; */
  CHECK_NONE  = 0, CHECK_CLINVAR = 1, CHECK_OBJINVAR = 2,
  CHECK_PRE   = 4, CHECK_POST = 8,
  CHECK_INVAR = CHECK_CLINVAR + CHECK_OBJINVAR,
  CHECK_ALL   = CHECK_INVAR   + CHECK_PRE + CHECK_POST
} CheckOptions;

void NsfAssertionRename(Tcl_Interp *interp, Tcl_Command cmd,
			  NsfAssertionStore *as,
			  char *oldSimpleCmdName, char *newName);
/*
 * mixins
 */
typedef struct NsfMixinStack {
  Tcl_Command currentCmdPtr;
  struct NsfMixinStack *nextPtr;
} NsfMixinStack;

/*
 * Generic command pointer list
 */
typedef struct NsfCmdList {
  Tcl_Command cmdPtr;
  ClientData clientData;
  struct NsfClass *clorobj;
  struct NsfCmdList *nextPtr;
} NsfCmdList;

typedef void (NsfFreeCmdListClientData) _ANSI_ARGS_((NsfCmdList*));

/* for incr string */
typedef struct NsfStringIncrStruct {
  char *buffer;
  char *start;
  size_t bufSize;
  int length;
} NsfStringIncrStruct;


/* 
 * cmd flags
 */

#define NSF_CMD_CALL_PROTECTED_METHOD 		0x00010000
#define NSF_CMD_CALL_PRIVATE_METHOD		0x00020000
#define NSF_CMD_REDEFINE_PROTECTED_METHOD	0x00040000
/* NSF_CMD_NONLEAF_METHOD is used to flag, if a Method implemented via cmd calls "next" */
#define NSF_CMD_NONLEAF_METHOD			0x00080000
#define NSF_CMD_CLASS_ONLY_METHOD		0x00100000
/*
 * object flags ...
 */

/* DESTROY_CALLED indicates that destroy was called on obj */
#define NSF_DESTROY_CALLED                 0x0001
/* INIT_CALLED indicates that init was called on obj */
#define NSF_INIT_CALLED                    0x0002
/* MIXIN_ORDER_VALID set when mixin order is valid */
#define NSF_MIXIN_ORDER_VALID              0x0004
/* MIXIN_ORDER_DEFINED set, when mixins are defined for obj */
#define NSF_MIXIN_ORDER_DEFINED            0x0008
#define NSF_MIXIN_ORDER_DEFINED_AND_VALID  0x000c
/* FILTER_ORDER_VALID set, when filter order is valid */
#define NSF_FILTER_ORDER_VALID             0x0010
/* FILTER_ORDER_DEFINED set, when filters are defined for obj */
#define NSF_FILTER_ORDER_DEFINED           0x0020
#define NSF_FILTER_ORDER_DEFINED_AND_VALID 0x0030
/* class and object properties for objects */
#define NSF_IS_CLASS                       0x0040
#define NSF_IS_ROOT_META_CLASS             0x0080
#define NSF_IS_ROOT_CLASS                  0x0100
#define NSF_IS_SLOT_CONTAINER              0x0200
#define NSF_KEEP_CALLER_SELF               0x0400
#define NSF_PER_OBJECT_DISPATCH            0x0800
#define NSF_HAS_PER_OBJECT_SLOTS           0x1000
/* deletion states */
#define NSF_DESTROY_CALLED_SUCCESS       0x010000 /* requires flags to be int, not short */
#define NSF_DURING_DELETE                0x020000
#define NSF_DELETED                      0x040000
#define NSF_RECREATE                     0x080000
#define NSF_TCL_DELETE                   0x100000  


/* method invocations */
#define NSF_ARG_METHOD_INVOCATION	     (NSF_ARG_ALIAS|NSF_ARG_FORWARD|NSF_ARG_INITCMD|NSF_ARG_CMD)
#define NSF_ARG_METHOD_CALL		     (NSF_ARG_ALIAS|NSF_ARG_FORWARD)

/* Disallowed parameter options */
#define NSF_DISALLOWED_ARG_METHOD_PARAMETER  (NSF_ARG_METHOD_INVOCATION|NSF_ARG_NOCONFIG|NSF_ARG_SLOTASSIGN|NSF_ARG_SLOTINITIALIZE)
#define NSF_DISALLOWED_ARG_SETTER	     (NSF_ARG_SWITCH|NSF_ARG_SUBST_DEFAULT|NSF_DISALLOWED_ARG_METHOD_PARAMETER)
/*#define NSF_DISALLOWED_ARG_OBJECT_PARAMETER  (NSF_ARG_SWITCH)*/
#define NSF_DISALLOWED_ARG_OBJECT_PARAMETER  0
#define NSF_DISALLOWED_ARG_VALUECHECK	     (NSF_ARG_SUBST_DEFAULT|NSF_ARG_METHOD_INVOCATION|NSF_ARG_SWITCH|NSF_ARG_CURRENTLY_UNKNOWN|NSF_ARG_SLOTASSIGN|NSF_ARG_SLOTINITIALIZE)

/* flags for ParseContext */
#define NSF_PC_MUST_DECR		     0x0001
#define NSF_PC_IS_DEFAULT		     0x0002
#define NSF_PC_INVERT_DEFAULT	     	     0x0010

#define NSF_PC_STATUS_MUST_DECR		     0x0001
#define NSF_PC_STATUS_FREE_OBJV		     0x0002
#define NSF_PC_STATUS_FREE_CD		     0x0004


/* method types */
#define NSF_METHODTYPE_ALIAS     0x0001
#define NSF_METHODTYPE_SCRIPTED  0x0002
#define NSF_METHODTYPE_SETTER    0x0004
#define NSF_METHODTYPE_FORWARDER 0x0008
#define NSF_METHODTYPE_OBJECT    0x0010
#define NSF_METHODTYPE_NSFPROC   0x0020
#define NSF_METHODTYPE_OTHER     0x0100
#define NSF_METHODTYPE_BUILTIN   NSF_METHODTYPE_ALIAS|NSF_METHODTYPE_SETTER|NSF_METHODTYPE_FORWARDER|NSF_METHODTYPE_OTHER
#define NSF_METHODTYPE_ALL       NSF_METHODTYPE_SCRIPTED|NSF_METHODTYPE_BUILTIN|NSF_METHODTYPE_OBJECT


#define NsfObjectSetClass(obj) \
	(obj)->flags |= NSF_IS_CLASS
#define NsfObjectClearClass(obj) \
	(obj)->flags &= ~NSF_IS_CLASS
#define NsfObjectIsClass(obj) \
	((obj)->flags & NSF_IS_CLASS)
#define NsfObjectToClass(obj) \
	(NsfClass*)((((NsfObject*)obj)->flags & NSF_IS_CLASS)?obj:NULL)


/*
 * object and class internals
 */

typedef struct NsfParamDefs {
  Nsf_Param *paramsPtr;
  int nrParams;
  int refCount;
  int serial;
  Tcl_Obj *slotObj;
  Tcl_Obj *returns;
} NsfParamDefs;

typedef struct NsfParsedParam {
  NsfParamDefs *paramDefs;
  int possibleUnknowns;
} NsfParsedParam;

typedef struct NsfObjectOpt {
  NsfAssertionStore *assertions;
  NsfCmdList *objFilters;
  NsfCmdList *objMixins;
  ClientData clientData;
#if defined(PER_OBJECT_PARAMETER_CACHING)
  NsfParsedParam *parsedParamPtr;
  int classParamPtrEpoch;
#endif
  CONST char *volatileVarName;
  short checkoptions;
} NsfObjectOpt;

typedef struct NsfObject {
  Tcl_Obj *cmdName;
  Tcl_Command id;
  Tcl_Interp *teardown;
  struct NsfClass *cl;
  TclVarHashTable *varTablePtr;
  Tcl_Namespace *nsPtr;
  NsfObjectOpt *opt;
  struct NsfCmdList *filterOrder;
  struct NsfCmdList *mixinOrder;
  NsfFilterStack *filterStack;
  NsfMixinStack *mixinStack;
  int refCount;
  unsigned int flags;
  short activationCount;
} NsfObject;

typedef struct NsfClassOpt {
  NsfCmdList *classFilters;
  NsfCmdList *classMixins;
  NsfCmdList *isObjectMixinOf;
  NsfCmdList *isClassMixinOf;
  NsfAssertionStore *assertions;
#ifdef NSF_OBJECTDATA
  Tcl_HashTable *objectdata;
#endif
  Tcl_Command id;
  ClientData clientData;
} NsfClassOpt;

typedef struct NsfClass {
  struct NsfObject object;
  struct NsfClasses *super;
  struct NsfClasses *sub;
  struct NsfObjectSystem *osPtr;
  struct NsfClasses *order;
  Tcl_HashTable instances;
  Tcl_Namespace *nsPtr;
  NsfParsedParam *parsedParamPtr;
#if defined(PER_OBJECT_PARAMETER_CACHING)
  int classParamPtrEpoch;
#endif
  NsfClassOpt *opt;
  short color;
} NsfClass;

typedef struct NsfClasses {
  struct NsfClass *cl;
  ClientData clientData;
  struct NsfClasses *nextPtr;
} NsfClasses;

/*
 * needed in nsf.c and in nsfShadow
 */
typedef struct NsfProcClientData {
  Tcl_Obj *procName;
  Tcl_Command cmd;
  NsfParamDefs *paramDefs;
  int with_ad;
  int checkAlwaysFlag;
} NsfProcClientData;

typedef enum SystemMethodsIdx {
  NSF_c_alloc_idx, 
  NSF_c_create_idx, 
  NSF_c_dealloc_idx,
  NSF_c_objectparameter_idx, 
  NSF_c_recreate_idx, 
  NSF_o_cleanup_idx, 
  NSF_o_configure_idx, 
  NSF_o_defaultmethod_idx, 
  NSF_o_destroy_idx, 
  NSF_o_init_idx, 
  NSF_o_move_idx, 
  NSF_o_unknown_idx
} SystemMethodsIdx;

#if !defined(NSF_C)
EXTERN CONST char *Nsf_SystemMethodOpts[];
#else 
CONST char *Nsf_SystemMethodOpts[] = {
  "-class.alloc", 
  "-class.create", 
  "-class.dealloc",
  "-class.objectparameter", 
  "-class.recreate", 
  "-object.cleanup", 
  "-object.configure", 
  "-object.defaultmethod", 
  "-object.destroy", 
  "-object.init", 
  "-object.move", 
  "-object.unknown",  
  NULL
};
#endif

typedef struct NsfObjectSystem {
  NsfClass *rootClass;
  NsfClass *rootMetaClass;
  int overloadedMethods;
  int definedMethods;
  Tcl_Obj *methods[NSF_o_unknown_idx+1];
  Tcl_Obj *handles[NSF_o_unknown_idx+1];
  struct NsfObjectSystem *nextPtr;
} NsfObjectSystem;


/* 
 * Next Scripting global names and strings 
 *
 * We provide enums for efficient lookup for corresponding string
 * names and Tcl_Objs via global arrays. The "constant" Tcl_Objs are
 * built at start-up-time via Nsf_Init().
 */

typedef enum {
  NSF_EMPTY, NSF_ZERO, NSF_ONE,
  /* methods called internally */
  NSF_CONFIGURE, NSF_INITIALIZE, NSF_ASSIGN, NSF_GET_PARAMETER_SPEC,
  /* var names */
  NSF_AUTONAMES, NSF_DEFAULTMETACLASS, NSF_DEFAULTSUPERCLASS, 
  NSF_ARRAY_INITCMD, NSF_ARRAY_CMD,
  NSF_ARRAY_ALIAS, NSF_ARRAY_PARAMETERSYNTAX, 
  NSF_POSITION, NSF_POSITIONAL, NSF_CONFIGURABLE, NSF_PARAMETERSPEC,
  /* object/class names */
  NSF_METHOD_PARAMETER_SLOT_OBJ, 
  /* constants */
  NSF_ALIAS, NSF_ARGS, NSF_CMD, NSF_FILTER, NSF_FORWARD, 
  NSF_METHOD,  NSF_OBJECT, NSF_SETTER, NSF_SETTERNAME, NSF_VALUECHECK,
  NSF_GUARD_OPTION, NSF___UNKNOWN__, NSF_ARRAY, NSF_GET, NSF_SET,
  NSF_OBJECT_UNKNOWN_HANDLER, NSF_ARGUMENT_UNKNOWN_HANDLER,
  /* Partly redefined Tcl commands; leave them together at the end */
  NSF_EXPR, NSF_FORMAT, NSF_INFO_BODY, NSF_INFO_FRAME, NSF_INTERP, 
  NSF_IS, NSF_EVAL,
  NSF_RENAME
} NsfGlobalNames;
#if !defined(NSF_C)
EXTERN char *NsfGlobalStrings[];
#else
char *NsfGlobalStrings[] = {
  "", "0", "1", 
  /* methods called internally */
  "configure", "initialize", "assign", "getParameterSpec",
  /* var names */
  "__autonames", "__default_metaclass", "__default_superclass", "__initcmd", "__cmd",
  "::nsf::alias", "::nsf::parameter::syntax", 
  "position", "positional", "configurable", "parameterSpec",
  /* object/class names */
  "::nx::methodParameterSlot", 
  /* constants */
  "alias", "args", "cmd", "filter",  "forward", 
  "method", "object", "setter", "settername", "valuecheck",
  "-guard", "__unknown__", "::array", "get", "set",
  /* nsf tcl commands */
  "::nsf::object::unknown",
  "::nsf::argument::unknown",
  /* tcl commands */
  "expr", "format", "::tcl::info::body", "::tcl::info::frame", "interp", 
  "::tcl::string::is", "::eval",
  "rename"
};
#endif

#define NsfGlobalObjs RUNTIME_STATE(interp)->methodObjNames

/* obj types */
EXTERN Tcl_ObjType NsfMixinregObjType;
int NsfMixinregGet(Tcl_Obj *obj, NsfClass **clPtr, Tcl_Obj **guardObj);

EXTERN Tcl_ObjType NsfFilterregObjType;
int NsfFilterregGet(Tcl_Obj *obj, Tcl_Obj **filterObj, Tcl_Obj **guardObj);

/* Next Scripting ShadowTclCommands */
typedef struct NsfShadowTclCommandInfo {
  TclObjCmdProcType proc;
  ClientData clientData;
} NsfShadowTclCommandInfo;
typedef enum {SHADOW_LOAD=1, SHADOW_UNLOAD=0, SHADOW_REFETCH=2} NsfShadowOperations;


typedef enum {NSF_PARAMS_NAMES, NSF_PARAMS_LIST, 
	      NSF_PARAMS_PARAMETER, NSF_PARAMS_SYNTAX} NsfParamsPrintStyle;

int NsfCallCommand(Tcl_Interp *interp, NsfGlobalNames name,
		     int objc, Tcl_Obj *CONST objv[]);
int NsfShadowTclCommands(Tcl_Interp *interp, NsfShadowOperations load);
Tcl_Obj * NsfMethodObj(NsfObject *object, int methodIdx);


/*
 * Next Scripting CallStack
 */
typedef struct NsfCallStackContent {
  NsfObject *self;
  NsfClass *cl;
  Tcl_Command cmdPtr;
  NsfFilterStack *filterStackEntry;
  Tcl_Obj *CONST* objv;
  int objc;
  unsigned int flags;
  unsigned short frameType;
#if defined(NSF_PROFILE) || defined(NSF_DTRACE)
  long int startUsec;
  long int startSec;
  CONST char *methodName;
#endif
} NsfCallStackContent;

#define NSF_CSC_TYPE_PLAIN              0
#define NSF_CSC_TYPE_ACTIVE_MIXIN       1
#define NSF_CSC_TYPE_ACTIVE_FILTER      2
#define NSF_CSC_TYPE_INACTIVE           4
#define NSF_CSC_TYPE_INACTIVE_MIXIN     5
#define NSF_CSC_TYPE_INACTIVE_FILTER    6
#define NSF_CSC_TYPE_GUARD           0x10
#define NSF_CSC_TYPE_ENSEMBLE        0x20

#define NSF_CSC_CALL_IS_NEXT               1
#define NSF_CSC_CALL_IS_GUARD              2
#define NSF_CSC_CALL_IS_ENSEMBLE           4
#define NSF_CSC_CALL_IS_COMPILE            8


#define NSF_CSC_IMMEDIATE           0x000100
#define NSF_CSC_FORCE_FRAME         0x000200
#define NSF_CSC_CALL_NO_UNKNOWN     0x000400
#define NSF_CSC_CALL_IS_NRE         0x002000
#define NSF_CSC_MIXIN_STACK_PUSHED  0x004000
#define NSF_CSC_FILTER_STACK_PUSHED 0x008000
#define NSF_CSC_METHOD_IS_UNKNOWN   0x010000 

/* flags for call method */
#define NSF_CM_NO_UNKNOWN           0x000001
#define NSF_CM_NO_SHIFT             0x000002
#define NSF_CM_IGNORE_PERMISSIONS   0x000004
#define NSF_CM_NO_OBJECT_METHOD     0x000008
#define NSF_CM_SYSTEM_METHOD        0x000010
#define NSF_CM_LOCAL_METHOD         0x000020
#define NSF_CM_INTRINSIC_METHOD     0x000040
#define NSF_CM_KEEP_CALLER_SELF     0x000080

#define NSF_CSC_COPY_FLAGS          (NSF_CSC_MIXIN_STACK_PUSHED|NSF_CSC_FILTER_STACK_PUSHED|NSF_CSC_IMMEDIATE|NSF_CSC_FORCE_FRAME|NSF_CM_LOCAL_METHOD)

#define NSF_VAR_TRIGGER_TRACE    1
#define NSF_VAR_REQUIRE_DEFINED  2
#define NSF_VAR_ISARRAY          4

/*
 * Tcl uses 01 and 02, TclOO uses 04 and 08, so leave some space free
 * for further extensions of tcl and tcloo...
 */
#define FRAME_IS_NSF_OBJECT  0x10000
#define FRAME_IS_NSF_METHOD  0x20000
#define FRAME_IS_NSF_CMETHOD 0x40000

#if defined(NRE)
# define NRE_SANE_PATCH 1
# define NsfImmediateFromCallerFlags(flags) \
  (((flags) & (NSF_CSC_CALL_IS_NRE|NSF_CSC_IMMEDIATE)) == NSF_CSC_CALL_IS_NRE ? 0 : NSF_CSC_IMMEDIATE)
# if defined(NRE_SANE_PATCH)
#  define NsfNRRunCallbacks(interp, result, rootPtr) TclNRRunCallbacks(interp, result, rootPtr)
#  if !defined(TclStackFree)
#   define TclStackFree(interp, ptr) ckfree(ptr)
#   define TclStackAlloc(interp, size) ckalloc(size)
#  endif
# else
#  define NsfNRRunCallbacks(interp, result, rootPtr) TclNRRunCallbacks(interp, result, rootPtr, 0)
#  define TEOV_callback NRE_callback
# endif
#endif

#if defined(NSF_PROFILE)
typedef struct NsfProfile {
  long int overallTime;
  long int startSec;
  long int startUSec;
  Tcl_HashTable objectData;
  Tcl_HashTable methodData;
  Tcl_HashTable procData;
} NsfProfile;
#endif

typedef struct NsfRuntimeState {
  /*
   * The defined object systems
   */
  struct NsfObjectSystem *objectSystems;
  /*
   * namespaces and cmds
   */ 
  Tcl_Namespace *NsfNS;           /* the ::nsf namespace */
  Tcl_Namespace *NsfClassesNS;    /* the ::nsf::classes namespace, where classes are created physically */
  Tcl_ObjCmdProc *objInterpProc;  /* cached result of TclGetObjInterpProc() */
  Tcl_Command colonCmd;           /* cmdPtr of cmd ":" to dispatch via cmdResolver */
  Proc fakeProc;                  /* dummy proc strucure, used for C-implemented methods with local scope */
  Tcl_Command currentMixinCmdPtr; /* cmdPtr of currently active mixin, used for "info activemixin" */
  int objectMethodEpoch;
  int instanceMethodEpoch;
#if defined(PER_OBJECT_PARAMETER_CACHING)
  int classParamPtrEpoch;
#endif
  Tcl_Obj **methodObjNames;       /* global objects of nsf */
  struct NsfShadowTclCommandInfo *tclCommands; /* shadowed Tcl commands */

#if defined(CHECK_ACTIVATION_COUNTS)
  NsfClasses *cscList;
#endif
  int errorCount;        /* keep track of number of errors to avoid potential error loops */
  int unknown;           /* keep track whether an unknown method is currently called */
  int overloadedMethods; /* bitarray for tracking overloaded methods */
  /* 
   * Configure options. The following do*-flags could be moved into a
   * bitarray, but we have only one state per interp, so the win on
   * memory is very little.
   */
  int debugLevel;
  int doCheckArguments;
  int doCheckResults;
  int doFilters;
  int doKeepcmds;
  int doProfile;
  int doSoftrecreate;
  /* keep track of defined filters */
  Tcl_HashTable activeFilterTablePtr;

  /*
   * shutdown handling
   */
  int exitHandlerDestroyRound;

#if defined(NSF_PROFILE)
  NsfProfile profile;
#endif
#if defined(NSF_STACKCHECK)
  void *bottomOfStack;
  void *maxStack;
#endif
  NsfStringIncrStruct iss; /* used for new to create new symbols */
  short guardCount;        /* keep track of guard invocations */
  ClientData clientData;
} NsfRuntimeState;

#define NSF_EXITHANDLER_OFF 0
#define NSF_EXITHANDLER_ON_SOFT_DESTROY 1
#define NSF_EXITHANDLER_ON_PHYSICAL_DESTROY 2


#ifdef NSF_OBJECTDATA
EXTERN void
NsfSetObjectData(struct NsfObject *obj, struct NsfClass *cl, ClientData data);
EXTERN int
NsfGetObjectData(struct NsfObject *obj, struct NsfClass *cl, ClientData *data);
EXTERN int
NsfUnsetObjectData(struct NsfObject *obj, struct NsfClass *cl);
EXTERN void
NsfFreeObjectData(NsfClass *cl);
#endif

/*
 *  NsfObject Reference Accounting
 */
#if defined(NSFOBJ_TRACE)
# define NsfObjectRefCountIncr(obj)					\
  ((NsfObject *)obj)->refCount++;					\
  fprintf(stderr, "RefCountIncr %p count=%d %s\n", obj, ((NsfObject *)obj)->refCount, \
	((NsfObject *)obj)->cmdName?ObjStr(((NsfObject *)obj)->cmdName):"no name"); \
  MEM_COUNT_ALLOC("NsfObject.refCount", obj)
# define NsfObjectRefCountDecr(obj)					\
  (obj)->refCount--;							\
  fprintf(stderr, "RefCountDecr %p count=%d\n", obj, obj->refCount);	\
  MEM_COUNT_FREE("NsfObject.refCount", obj)
#else
# define NsfObjectRefCountIncr(obj)           \
  (obj)->refCount++;                            \
  MEM_COUNT_ALLOC("NsfObject.refCount", obj)
# define NsfObjectRefCountDecr(obj)           \
  (obj)->refCount--;                            \
  MEM_COUNT_FREE("NsfObject.refCount", obj)
#endif

/*
 *
 *  internally used API functions
 *
 */

#include "nsfIntDecls.h"

/*
 * Profiling functions
 */

#if defined(NSF_PROFILE)
EXTERN void NsfProfileRecordMethodData(Tcl_Interp* interp, NsfCallStackContent *cscPtr);
EXTERN void NsfProfileRecordProcData(Tcl_Interp *interp, char *methodName, long startSec, long startUsec);
EXTERN void NsfProfileInit(Tcl_Interp *interp);
EXTERN void NsfProfileFree(Tcl_Interp *interp);
EXTERN void NsfProfileClearData(Tcl_Interp *interp);
EXTERN void NsfProfileGetData(Tcl_Interp *interp);
EXTERN NsfCallStackContent *NsfCallStackGetTopFrame(Tcl_Interp *interp, Tcl_CallFrame **framePtrPtr);
#endif

/*
 * MEM Counting
 */
#ifdef NSF_MEM_COUNT
void NsfMemCountAlloc(char *id, void *);
void NsfMemCountFree(char *id, void *);
void NsfMemCountInit();
void NsfMemCountRelease();
#endif /* NSF_MEM_COUNT */

/*
 * TCL_STACK_ALLOC_TRACE
 */
#if defined(TCL_STACK_ALLOC_TRACE)
# define NsfTclStackFree(interp,ptr,msg) \
  fprintf(stderr, "---- TclStackFree %p %s\n", ptr, msg);\
  TclStackFree(interp,ptr)

static char *
NsfTclStackAlloc(Tcl_Interp *interp, size_t size, char *msg) {
  char *ptr = TclStackAlloc(interp, size);
  fprintf(stderr, "---- TclStackAlloc %p %s\n", ptr, msg);
  return ptr;
}
#else
# define NsfTclStackFree(interp,ptr,msg) TclStackFree(interp,ptr)
# define NsfTclStackAlloc(interp,size,msg) TclStackAlloc(interp,size)
#endif

/*
 * bytecode support
 */
#ifdef NSF_BYTECODE
typedef struct NsfCompEnv {
  int bytecode;
  Command *cmdPtr;
  CompileProc *compileProc;
  Tcl_ObjCmdProc *callProc;
} NsfCompEnv;

typedef enum {INST_INITPROC, INST_NEXT, INST_SELF, INST_SELF_DISPATCH,
	      LAST_INSTRUCTION} NsfByteCodeInstructions;

Tcl_ObjCmdProc NsfInitProcNSCmd, NsfSelfDispatchCmd,
  NsfNextObjCmd, NsfGetSelfObjCmd;

EXTERN NsfCompEnv *NsfGetCompEnv();
int NsfDirectSelfDispatch(ClientData cd, Tcl_Interp *interp,
		     int objc, Tcl_Obj *CONST objv[]);
#endif

EXTERN int NsfGetClassFromObj(Tcl_Interp *interp, Tcl_Obj *objPtr,
			      NsfClass **clPtr, int withUnknown);
EXTERN int NsfObjDispatch(ClientData cd, Tcl_Interp *interp,
			  int objc, Tcl_Obj *CONST objv[]);
EXTERN int NsfObjWrongArgs(Tcl_Interp *interp, CONST char *msg, 
			   Tcl_Obj *cmdName, Tcl_Obj *methodName, 
			   char *arglist);
EXTERN CONST char *NsfMethodName(Tcl_Obj *methodObj);
EXTERN void NsfReportVars(Tcl_Interp *interp);
EXTERN void NsfDStringArgv(Tcl_DString *dsPtr, int objc, Tcl_Obj *CONST objv[]);

EXTERN Tcl_Obj *NsfMethodNamePath(Tcl_Interp *interp, 
				  Tcl_CallFrame *framePtr, 
				  CONST char *methodName);

/*
 * Definition of methodEpoch macros
 */
#if defined(METHOD_OBJECT_TRACE)
# define NsfInstanceMethodEpochIncr(msg) \
  RUNTIME_STATE(interp)->instanceMethodEpoch++;	\
  fprintf(stderr, "+++ instanceMethodEpoch %d %s\n", RUNTIME_STATE(interp)->instanceMethodEpoch, msg)
# define NsfObjectMethodEpochIncr(msg) \
  RUNTIME_STATE(interp)->objectMethodEpoch++;	\
  fprintf(stderr, "+++ objectMethodEpoch %d %s\n", RUNTIME_STATE(interp)->objectMethodEpoch, msg)
#else
# define NsfInstanceMethodEpochIncr(msg) RUNTIME_STATE(interp)->instanceMethodEpoch++
# define NsfObjectMethodEpochIncr(msg)   RUNTIME_STATE(interp)->objectMethodEpoch++
#endif

#if defined(PER_OBJECT_PARAMETER_CACHING)
# define NsfClassParamPtrEpochIncr(msg)   RUNTIME_STATE(interp)->classParamPtrEpoch++
#else
# define NsfClassParamPtrEpochIncr(msg)
#endif

/* 
 * NsfFlag type
 */
EXTERN Tcl_ObjType NsfFlagObjType;
EXTERN int NsfFlagObjSet(Tcl_Interp *interp, Tcl_Obj *objPtr, 
			 Nsf_Param CONST *baseParamPtr, int serial,
			 Nsf_Param CONST *paramPtr, Tcl_Obj *payload, int flags);
typedef struct {
  CONST Nsf_Param *signature;
  int serial;
  Nsf_Param CONST *paramPtr;
  Tcl_Obj *payload;
  int flags;
} NsfFlag;

#define NSF_FLAG_DASHDAH		0x01
#define NSF_FLAG_CONTAINS_VALUE		0x02

/* 
 * NsfMethodContext type
 */
EXTERN Tcl_ObjType NsfInstanceMethodObjType;
EXTERN Tcl_ObjType NsfObjectMethodObjType;
EXTERN int NsfMethodObjSet(Tcl_Interp *interp, Tcl_Obj *objPtr, 
			   Tcl_ObjType *objectType,
			   void *context, int methodEpoch,
			   Tcl_Command cmd, NsfClass *cl, int flags);

typedef struct {
  void *context;
  int methodEpoch;
  Tcl_Command cmd;
  NsfClass *cl;
  int flags;
} NsfMethodContext;

/* functions from nsfUtil.c */
char *Nsf_ltoa(char *buf, long i, int *len);
char *NsfStringIncr(NsfStringIncrStruct *iss);
void NsfStringIncrInit(NsfStringIncrStruct *iss);
void NsfStringIncrFree(NsfStringIncrStruct *iss);

#ifndef HAVE_STRNSTR
char *strnstr(const char *buffer, const char *needle, size_t buffer_len);
#endif

/* 
   In ANSI mode (ISO C89/90) compilers such as gcc and clang do not
   define the va_copy macro. However, they *do* in reasonably recent
   versions provide a prefixed (__*) one. The by-feature test below
   falls back to the prefixed version, if available, and provides a
   more general fallback to a simple assignment; this is primarily for
   MSVC; admittedly, this simplifcation is not generally portable to
   platform/compiler combos other then x86, but the best I can think of right
   now. One might constrain the assignment-fallback to a platform and 
   leave va_copy undefined in uncaught platform cases (?).
*/
#ifndef va_copy
#ifdef	__va_copy
#define	va_copy(dest,src) __va_copy(dest,src)
#else
#define va_copy(dest,src) ((dest) = (src))
#endif
#endif

#if !defined(NDEBUG)
/*# define NSF_INLINE*/
#endif

#endif /* _nsf_int_h_ */
