
static int convertToInfomethodsubcmd(Tcl_Interp *interp, Tcl_Obj *objPtr, XOTclParam CONST *pPtr, ClientData *clientData) {
  int index, result;
  static CONST char *opts[] = {"definition", "name", "parameter", "type", NULL};
  result = Tcl_GetIndexFromObj(interp, objPtr, opts, "infomethodsubcmd", 0, &index);
  *clientData = (ClientData) index + 1;
  return result;
}
enum InfomethodsubcmdIdx {InfomethodsubcmdNULL, InfomethodsubcmdDefinitionIdx, InfomethodsubcmdNameIdx, InfomethodsubcmdParameterIdx, InfomethodsubcmdTypeIdx};
  
static int convertToMethodtype(Tcl_Interp *interp, Tcl_Obj *objPtr, XOTclParam CONST *pPtr, ClientData *clientData) {
  int index, result;
  static CONST char *opts[] = {"all", "scripted", "system", "alias", "forwarder", "object", "setter", NULL};
  result = Tcl_GetIndexFromObj(interp, objPtr, opts, "-methodtype", 0, &index);
  *clientData = (ClientData) index + 1;
  return result;
}
enum MethodtypeIdx {MethodtypeNULL, MethodtypeAllIdx, MethodtypeScriptedIdx, MethodtypeSystemIdx, MethodtypeAliasIdx, MethodtypeForwarderIdx, MethodtypeObjectIdx, MethodtypeSetterIdx};
  
static int convertToConfigureoption(Tcl_Interp *interp, Tcl_Obj *objPtr, XOTclParam CONST *pPtr, ClientData *clientData) {
  int index, result;
  static CONST char *opts[] = {"filter", "softrecreate", "cacheinterface", NULL};
  result = Tcl_GetIndexFromObj(interp, objPtr, opts, "configureoption", 0, &index);
  *clientData = (ClientData) index + 1;
  return result;
}
enum ConfigureoptionIdx {ConfigureoptionNULL, ConfigureoptionFilterIdx, ConfigureoptionSoftrecreateIdx, ConfigureoptionCacheinterfaceIdx};
  
static int convertToSelfoption(Tcl_Interp *interp, Tcl_Obj *objPtr, XOTclParam CONST *pPtr, ClientData *clientData) {
  int index, result;
  static CONST char *opts[] = {"proc", "class", "activelevel", "args", "activemixin", "calledproc", "calledmethod", "calledclass", "callingproc", "callingclass", "callinglevel", "callingobject", "filterreg", "isnextcall", "next", NULL};
  result = Tcl_GetIndexFromObj(interp, objPtr, opts, "selfoption", 0, &index);
  *clientData = (ClientData) index + 1;
  return result;
}
enum SelfoptionIdx {SelfoptionNULL, SelfoptionProcIdx, SelfoptionClassIdx, SelfoptionActivelevelIdx, SelfoptionArgsIdx, SelfoptionActivemixinIdx, SelfoptionCalledprocIdx, SelfoptionCalledmethodIdx, SelfoptionCalledclassIdx, SelfoptionCallingprocIdx, SelfoptionCallingclassIdx, SelfoptionCallinglevelIdx, SelfoptionCallingobjectIdx, SelfoptionFilterregIdx, SelfoptionIsnextcallIdx, SelfoptionNextIdx};
  
static int convertToObjectkind(Tcl_Interp *interp, Tcl_Obj *objPtr, XOTclParam CONST *pPtr, ClientData *clientData) {
  int index, result;
  static CONST char *opts[] = {"type", "object", "class", "metaclass", "mixin", NULL};
  result = Tcl_GetIndexFromObj(interp, objPtr, opts, "objectkind", 0, &index);
  *clientData = (ClientData) index + 1;
  return result;
}
enum ObjectkindIdx {ObjectkindNULL, ObjectkindTypeIdx, ObjectkindObjectIdx, ObjectkindClassIdx, ObjectkindMetaclassIdx, ObjectkindMixinIdx};
  
static int convertToMethodproperty(Tcl_Interp *interp, Tcl_Obj *objPtr, XOTclParam CONST *pPtr, ClientData *clientData) {
  int index, result;
  static CONST char *opts[] = {"protected", "static", "slotobj", NULL};
  result = Tcl_GetIndexFromObj(interp, objPtr, opts, "methodproperty", 0, &index);
  *clientData = (ClientData) index + 1;
  return result;
}
enum MethodpropertyIdx {MethodpropertyNULL, MethodpropertyProtectedIdx, MethodpropertyStaticIdx, MethodpropertySlotobjIdx};
  
static int convertToRelationtype(Tcl_Interp *interp, Tcl_Obj *objPtr, XOTclParam CONST *pPtr, ClientData *clientData) {
  int index, result;
  static CONST char *opts[] = {"mixin", "instmixin", "object-mixin", "class-mixin", "filter", "instfilter", "object-filter", "class-filter", "class", "superclass", "rootclass", NULL};
  result = Tcl_GetIndexFromObj(interp, objPtr, opts, "relationtype", 0, &index);
  *clientData = (ClientData) index + 1;
  return result;
}
enum RelationtypeIdx {RelationtypeNULL, RelationtypeMixinIdx, RelationtypeInstmixinIdx, RelationtypeObject_mixinIdx, RelationtypeClass_mixinIdx, RelationtypeFilterIdx, RelationtypeInstfilterIdx, RelationtypeObject_filterIdx, RelationtypeClass_filterIdx, RelationtypeClassIdx, RelationtypeSuperclassIdx, RelationtypeRootclassIdx};
  

typedef struct {
  char *methodName;
  Tcl_ObjCmdProc *proc;
  int nrParameters;
  XOTclParam paramDefs[11];
} methodDefinition;

static int ArgumentParse(Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[], 
                         XOTclObject *obj, Tcl_Obj *procName,
                         XOTclParam CONST *paramPtr, int nrParameters, parseContext *pc);

static int getMatchObject(Tcl_Interp *interp, Tcl_Obj *patternObj, Tcl_Obj *origObj,
			  XOTclObject **matchObject, char **pattern);

/* just to define the symbol */
static methodDefinition method_definitions[];
  
static char *method_command_namespace_names[] = {
  "::xotcl::cmd::ObjectInfo",
  "::xotcl::cmd::Object",
  "::xotcl::cmd::ClassInfo",
  "::xotcl::cmd::ParameterType",
  "::xotcl::cmd::Class"
};
static int XOTclCheckBooleanArgsStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclCheckRequiredArgsStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclCAllocMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclCCreateMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclCDeallocMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclCForwardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclCInstFilterGuardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclCInvalidateObjectParameterMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclCInvariantsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclCMethodMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclCMixinGuardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclCNewMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclCRecreateMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclCSetterMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclClassInfoAliasMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclClassInfoFilterMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclClassInfoFilterguardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclClassInfoHeritageMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclClassInfoInstancesMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclClassInfoInstforwardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclClassInfoInstinvarMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclClassInfoInstmixinofMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclClassInfoInstparamsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclClassInfoInstpostMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclClassInfoInstpreMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclClassInfoMethodMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclClassInfoMethodsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclClassInfoMixinMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclClassInfoMixinguardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclClassInfoMixinofMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclClassInfoParameterMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclClassInfoSlotsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclClassInfoSubclassMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclClassInfoSuperclassMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclObjInfoAliasMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclObjInfoCheckMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclObjInfoChildrenMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclObjInfoClassMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclObjInfoFilterMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclObjInfoFilterguardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclObjInfoForwardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclObjInfoHasnamespaceMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclObjInfoInvarMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclObjInfoMethodMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclObjInfoMethodsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclObjInfoMixinMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclObjInfoMixinguardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclObjInfoParamsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclObjInfoParentMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclObjInfoPostMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclObjInfoPreMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclObjInfoPrecedenceMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclObjInfoSlotObjectsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclObjInfoVarsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclOAutonameMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclOCheckMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclOCleanupMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclOConfigureMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclODestroyMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclOExistsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclOFilterGuardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclOFilterSearchMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclOForwardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclOInstVarMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclOInvariantsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclOMethodMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclOMixinGuardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclONextMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclONoinitMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclOProcSearchMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclORequireNamespaceMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclOResidualargsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclOSetterMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclOUplevelMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclOUpvarMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclOVolatileMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclOVwaitMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclAliasCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclConfigureCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclCreateObjectSystemCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclDeprecatedCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclDispatchCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclDotCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclFinalizeObjCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclGetSelfObjCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclInstvarCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclInterpObjCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclIsCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclMethodPropertyCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclMyCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclNSCopyCmdsStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclNSCopyVarsStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclQualifyObjCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclRelationCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);
static int XOTclSetInstvarCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv []);

static int XOTclCheckBooleanArgs(Tcl_Interp *interp, char *name, Tcl_Obj *value);
static int XOTclCheckRequiredArgs(Tcl_Interp *interp, char *name, Tcl_Obj *value);
static int XOTclCAllocMethod(Tcl_Interp *interp, XOTclClass *cl, Tcl_Obj *name);
static int XOTclCCreateMethod(Tcl_Interp *interp, XOTclClass *cl, char *name, int objc, Tcl_Obj *CONST objv[]);
static int XOTclCDeallocMethod(Tcl_Interp *interp, XOTclClass *cl, Tcl_Obj *object);
static int XOTclCForwardMethod(Tcl_Interp *interp, XOTclClass *cl, int withPer_object, Tcl_Obj *name, Tcl_Obj *withDefault, int withEarlybinding, Tcl_Obj *withMethodprefix, int withObjscope, Tcl_Obj *withOnerror, int withVerbose, Tcl_Obj *target, int nobjc, Tcl_Obj *CONST nobjv[]);
static int XOTclCInstFilterGuardMethod(Tcl_Interp *interp, XOTclClass *cl, char *filter, Tcl_Obj *guard);
static int XOTclCInvalidateObjectParameterMethod(Tcl_Interp *interp, XOTclClass *cl);
static int XOTclCInvariantsMethod(Tcl_Interp *interp, XOTclClass *cl, Tcl_Obj *invariantlist);
static int XOTclCMethodMethod(Tcl_Interp *interp, XOTclClass *cl, int withInner_namespace, int withPer_object, int withProtected, Tcl_Obj *name, Tcl_Obj *args, Tcl_Obj *body, Tcl_Obj *withPrecondition, Tcl_Obj *withPostcondition);
static int XOTclCMixinGuardMethod(Tcl_Interp *interp, XOTclClass *cl, int withPer_object, char *mixin, Tcl_Obj *guard);
static int XOTclCNewMethod(Tcl_Interp *interp, XOTclClass *cl, XOTclObject *withChildof, int nobjc, Tcl_Obj *CONST nobjv[]);
static int XOTclCRecreateMethod(Tcl_Interp *interp, XOTclClass *cl, Tcl_Obj *name, int objc, Tcl_Obj *CONST objv[]);
static int XOTclCSetterMethod(Tcl_Interp *interp, XOTclClass *cl, int withPer_object, char *name);
static int XOTclClassInfoAliasMethod(Tcl_Interp *interp, XOTclClass *object, int withDefinition, char *name);
static int XOTclClassInfoFilterMethod(Tcl_Interp *interp, XOTclClass *class, int withGuards, char *pattern);
static int XOTclClassInfoFilterguardMethod(Tcl_Interp *interp, XOTclClass *class, char *filter);
static int XOTclClassInfoHeritageMethod(Tcl_Interp *interp, XOTclClass *class, char *pattern);
static int XOTclClassInfoInstancesMethod(Tcl_Interp *interp, XOTclClass *class, int withClosure, char *patternString, XOTclObject *patternObj);
static int XOTclClassInfoInstforwardMethod(Tcl_Interp *interp, XOTclClass *class, int withDefinition, char *name);
static int XOTclClassInfoInstinvarMethod(Tcl_Interp *interp, XOTclClass *class);
static int XOTclClassInfoInstmixinofMethod(Tcl_Interp *interp, XOTclClass *class, int withClosure, char *patternString, XOTclObject *patternObj);
static int XOTclClassInfoInstparamsMethod(Tcl_Interp *interp, XOTclClass *class, char *methodName, int withVarnames);
static int XOTclClassInfoInstpostMethod(Tcl_Interp *interp, XOTclClass *class, char *methodName);
static int XOTclClassInfoInstpreMethod(Tcl_Interp *interp, XOTclClass *class, char *methodName);
static int XOTclClassInfoMethodMethod(Tcl_Interp *interp, XOTclClass *class, int infomethodsubcmd, char *name);
static int XOTclClassInfoMethodsMethod(Tcl_Interp *interp, XOTclClass *object, int withDefined, int withMethodtype, int withNomixins, int withIncontext, char *pattern);
static int XOTclClassInfoMixinMethod(Tcl_Interp *interp, XOTclClass *class, int withClosure, int withGuards, char *patternString, XOTclObject *patternObj);
static int XOTclClassInfoMixinguardMethod(Tcl_Interp *interp, XOTclClass *class, char *mixin);
static int XOTclClassInfoMixinofMethod(Tcl_Interp *interp, XOTclClass *class, int withClosure, char *patternString, XOTclObject *patternObj);
static int XOTclClassInfoParameterMethod(Tcl_Interp *interp, XOTclClass *class);
static int XOTclClassInfoSlotsMethod(Tcl_Interp *interp, XOTclClass *class);
static int XOTclClassInfoSubclassMethod(Tcl_Interp *interp, XOTclClass *class, int withClosure, char *patternString, XOTclObject *patternObj);
static int XOTclClassInfoSuperclassMethod(Tcl_Interp *interp, XOTclClass *class, int withClosure, Tcl_Obj *pattern);
static int XOTclObjInfoAliasMethod(Tcl_Interp *interp, XOTclObject *object, int withDefinition, char *name);
static int XOTclObjInfoCheckMethod(Tcl_Interp *interp, XOTclObject *object);
static int XOTclObjInfoChildrenMethod(Tcl_Interp *interp, XOTclObject *object, char *pattern);
static int XOTclObjInfoClassMethod(Tcl_Interp *interp, XOTclObject *object);
static int XOTclObjInfoFilterMethod(Tcl_Interp *interp, XOTclObject *object, int withOrder, int withGuards, char *pattern);
static int XOTclObjInfoFilterguardMethod(Tcl_Interp *interp, XOTclObject *object, char *filter);
static int XOTclObjInfoForwardMethod(Tcl_Interp *interp, XOTclObject *object, int withDefinition, char *name);
static int XOTclObjInfoHasnamespaceMethod(Tcl_Interp *interp, XOTclObject *object);
static int XOTclObjInfoInvarMethod(Tcl_Interp *interp, XOTclObject *object);
static int XOTclObjInfoMethodMethod(Tcl_Interp *interp, XOTclObject *object, int infomethodsubcmd, char *name);
static int XOTclObjInfoMethodsMethod(Tcl_Interp *interp, XOTclObject *object, int withDefined, int withMethodtype, int withNomixins, int withIncontext, char *pattern);
static int XOTclObjInfoMixinMethod(Tcl_Interp *interp, XOTclObject *object, int withGuards, int withOrder, char *patternString, XOTclObject *patternObj);
static int XOTclObjInfoMixinguardMethod(Tcl_Interp *interp, XOTclObject *object, char *mixin);
static int XOTclObjInfoParamsMethod(Tcl_Interp *interp, XOTclObject *object, char *methodName, int withVarnames);
static int XOTclObjInfoParentMethod(Tcl_Interp *interp, XOTclObject *object);
static int XOTclObjInfoPostMethod(Tcl_Interp *interp, XOTclObject *object, char *methodName);
static int XOTclObjInfoPreMethod(Tcl_Interp *interp, XOTclObject *object, char *methodName);
static int XOTclObjInfoPrecedenceMethod(Tcl_Interp *interp, XOTclObject *object, int withIntrinsic, char *pattern);
static int XOTclObjInfoSlotObjectsMethod(Tcl_Interp *interp, XOTclObject *object, char *pattern);
static int XOTclObjInfoVarsMethod(Tcl_Interp *interp, XOTclObject *object, char *pattern);
static int XOTclOAutonameMethod(Tcl_Interp *interp, XOTclObject *obj, int withInstance, int withReset, Tcl_Obj *name);
static int XOTclOCheckMethod(Tcl_Interp *interp, XOTclObject *obj, Tcl_Obj *flag);
static int XOTclOCleanupMethod(Tcl_Interp *interp, XOTclObject *obj);
static int XOTclOConfigureMethod(Tcl_Interp *interp, XOTclObject *obj, int objc, Tcl_Obj *CONST objv[]);
static int XOTclODestroyMethod(Tcl_Interp *interp, XOTclObject *obj);
static int XOTclOExistsMethod(Tcl_Interp *interp, XOTclObject *obj, char *var);
static int XOTclOFilterGuardMethod(Tcl_Interp *interp, XOTclObject *obj, char *filter, Tcl_Obj *guard);
static int XOTclOFilterSearchMethod(Tcl_Interp *interp, XOTclObject *obj, char *filter);
static int XOTclOForwardMethod(Tcl_Interp *interp, XOTclObject *obj, Tcl_Obj *method, Tcl_Obj *withDefault, int withEarlybinding, Tcl_Obj *withMethodprefix, int withObjscope, Tcl_Obj *withOnerror, int withVerbose, Tcl_Obj *target, int nobjc, Tcl_Obj *CONST nobjv[]);
static int XOTclOInstVarMethod(Tcl_Interp *interp, XOTclObject *obj, int objc, Tcl_Obj *CONST objv[]);
static int XOTclOInvariantsMethod(Tcl_Interp *interp, XOTclObject *obj, Tcl_Obj *invariantlist);
static int XOTclOMethodMethod(Tcl_Interp *interp, XOTclObject *obj, int withInner_namespace, int withProtected, Tcl_Obj *name, Tcl_Obj *args, Tcl_Obj *body, Tcl_Obj *withPrecondition, Tcl_Obj *withPostcondition);
static int XOTclOMixinGuardMethod(Tcl_Interp *interp, XOTclObject *obj, char *mixin, Tcl_Obj *guard);
static int XOTclONextMethod(Tcl_Interp *interp, XOTclObject *obj, int objc, Tcl_Obj *CONST objv[]);
static int XOTclONoinitMethod(Tcl_Interp *interp, XOTclObject *obj);
static int XOTclOProcSearchMethod(Tcl_Interp *interp, XOTclObject *obj, char *name);
static int XOTclORequireNamespaceMethod(Tcl_Interp *interp, XOTclObject *obj);
static int XOTclOResidualargsMethod(Tcl_Interp *interp, XOTclObject *obj, int objc, Tcl_Obj *CONST objv[]);
static int XOTclOSetterMethod(Tcl_Interp *interp, XOTclObject *obj, char *name);
static int XOTclOUplevelMethod(Tcl_Interp *interp, XOTclObject *obj, int objc, Tcl_Obj *CONST objv[]);
static int XOTclOUpvarMethod(Tcl_Interp *interp, XOTclObject *obj, int objc, Tcl_Obj *CONST objv[]);
static int XOTclOVolatileMethod(Tcl_Interp *interp, XOTclObject *obj);
static int XOTclOVwaitMethod(Tcl_Interp *interp, XOTclObject *obj, char *varname);
static int XOTclAliasCmd(Tcl_Interp *interp, XOTclObject *object, char *methodName, int withObjscope, int withPer_object, int withProtected, Tcl_Obj *cmdName);
static int XOTclConfigureCmd(Tcl_Interp *interp, int configureoption, Tcl_Obj *value);
static int XOTclCreateObjectSystemCmd(Tcl_Interp *interp, Tcl_Obj *rootClass, Tcl_Obj *rootMetaClass);
static int XOTclDeprecatedCmd(Tcl_Interp *interp, char *what, char *oldCmd, char *newCmd);
static int XOTclDispatchCmd(Tcl_Interp *interp, XOTclObject *object, int withObjscope, Tcl_Obj *command, int nobjc, Tcl_Obj *CONST nobjv[]);
static int XOTclDotCmd(Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]);
static int XOTclFinalizeObjCmd(Tcl_Interp *interp);
static int XOTclGetSelfObjCmd(Tcl_Interp *interp, int selfoption);
static int XOTclInstvarCmd(Tcl_Interp *interp, XOTclObject *withObject, int nobjc, Tcl_Obj *CONST nobjv[]);
static int XOTclInterpObjCmd(Tcl_Interp *interp, char *name, int objc, Tcl_Obj *CONST objv[]);
static int XOTclIsCmd(Tcl_Interp *interp, Tcl_Obj *object, int objectkind, Tcl_Obj *value);
static int XOTclMethodPropertyCmd(Tcl_Interp *interp, XOTclObject *object, char *methodName, int withPer_object, int methodproperty, Tcl_Obj *value);
static int XOTclMyCmd(Tcl_Interp *interp, int withLocal, Tcl_Obj *method, int nobjc, Tcl_Obj *CONST nobjv[]);
static int XOTclNSCopyCmds(Tcl_Interp *interp, Tcl_Obj *fromNs, Tcl_Obj *toNs);
static int XOTclNSCopyVars(Tcl_Interp *interp, Tcl_Obj *fromNs, Tcl_Obj *toNs);
static int XOTclQualifyObjCmd(Tcl_Interp *interp, Tcl_Obj *name);
static int XOTclRelationCmd(Tcl_Interp *interp, XOTclObject *object, int withPer_object, int relationtype, Tcl_Obj *value);
static int XOTclSetInstvarCmd(Tcl_Interp *interp, XOTclObject *object, Tcl_Obj *variable, Tcl_Obj *value);

enum {
 XOTclCheckBooleanArgsIdx,
 XOTclCheckRequiredArgsIdx,
 XOTclCAllocMethodIdx,
 XOTclCCreateMethodIdx,
 XOTclCDeallocMethodIdx,
 XOTclCForwardMethodIdx,
 XOTclCInstFilterGuardMethodIdx,
 XOTclCInvalidateObjectParameterMethodIdx,
 XOTclCInvariantsMethodIdx,
 XOTclCMethodMethodIdx,
 XOTclCMixinGuardMethodIdx,
 XOTclCNewMethodIdx,
 XOTclCRecreateMethodIdx,
 XOTclCSetterMethodIdx,
 XOTclClassInfoAliasMethodIdx,
 XOTclClassInfoFilterMethodIdx,
 XOTclClassInfoFilterguardMethodIdx,
 XOTclClassInfoHeritageMethodIdx,
 XOTclClassInfoInstancesMethodIdx,
 XOTclClassInfoInstforwardMethodIdx,
 XOTclClassInfoInstinvarMethodIdx,
 XOTclClassInfoInstmixinofMethodIdx,
 XOTclClassInfoInstparamsMethodIdx,
 XOTclClassInfoInstpostMethodIdx,
 XOTclClassInfoInstpreMethodIdx,
 XOTclClassInfoMethodMethodIdx,
 XOTclClassInfoMethodsMethodIdx,
 XOTclClassInfoMixinMethodIdx,
 XOTclClassInfoMixinguardMethodIdx,
 XOTclClassInfoMixinofMethodIdx,
 XOTclClassInfoParameterMethodIdx,
 XOTclClassInfoSlotsMethodIdx,
 XOTclClassInfoSubclassMethodIdx,
 XOTclClassInfoSuperclassMethodIdx,
 XOTclObjInfoAliasMethodIdx,
 XOTclObjInfoCheckMethodIdx,
 XOTclObjInfoChildrenMethodIdx,
 XOTclObjInfoClassMethodIdx,
 XOTclObjInfoFilterMethodIdx,
 XOTclObjInfoFilterguardMethodIdx,
 XOTclObjInfoForwardMethodIdx,
 XOTclObjInfoHasnamespaceMethodIdx,
 XOTclObjInfoInvarMethodIdx,
 XOTclObjInfoMethodMethodIdx,
 XOTclObjInfoMethodsMethodIdx,
 XOTclObjInfoMixinMethodIdx,
 XOTclObjInfoMixinguardMethodIdx,
 XOTclObjInfoParamsMethodIdx,
 XOTclObjInfoParentMethodIdx,
 XOTclObjInfoPostMethodIdx,
 XOTclObjInfoPreMethodIdx,
 XOTclObjInfoPrecedenceMethodIdx,
 XOTclObjInfoSlotObjectsMethodIdx,
 XOTclObjInfoVarsMethodIdx,
 XOTclOAutonameMethodIdx,
 XOTclOCheckMethodIdx,
 XOTclOCleanupMethodIdx,
 XOTclOConfigureMethodIdx,
 XOTclODestroyMethodIdx,
 XOTclOExistsMethodIdx,
 XOTclOFilterGuardMethodIdx,
 XOTclOFilterSearchMethodIdx,
 XOTclOForwardMethodIdx,
 XOTclOInstVarMethodIdx,
 XOTclOInvariantsMethodIdx,
 XOTclOMethodMethodIdx,
 XOTclOMixinGuardMethodIdx,
 XOTclONextMethodIdx,
 XOTclONoinitMethodIdx,
 XOTclOProcSearchMethodIdx,
 XOTclORequireNamespaceMethodIdx,
 XOTclOResidualargsMethodIdx,
 XOTclOSetterMethodIdx,
 XOTclOUplevelMethodIdx,
 XOTclOUpvarMethodIdx,
 XOTclOVolatileMethodIdx,
 XOTclOVwaitMethodIdx,
 XOTclAliasCmdIdx,
 XOTclConfigureCmdIdx,
 XOTclCreateObjectSystemCmdIdx,
 XOTclDeprecatedCmdIdx,
 XOTclDispatchCmdIdx,
 XOTclDotCmdIdx,
 XOTclFinalizeObjCmdIdx,
 XOTclGetSelfObjCmdIdx,
 XOTclInstvarCmdIdx,
 XOTclInterpObjCmdIdx,
 XOTclIsCmdIdx,
 XOTclMethodPropertyCmdIdx,
 XOTclMyCmdIdx,
 XOTclNSCopyCmdsIdx,
 XOTclNSCopyVarsIdx,
 XOTclQualifyObjCmdIdx,
 XOTclRelationCmdIdx,
 XOTclSetInstvarCmdIdx
} XOTclMethods;


static int
XOTclCheckBooleanArgsStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclCheckBooleanArgsIdx].paramDefs, 
                     method_definitions[XOTclCheckBooleanArgsIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    char *name = (char *)pc.clientData[0];
    Tcl_Obj *value = (Tcl_Obj *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclCheckBooleanArgs(interp, name, value);

  }
}

static int
XOTclCheckRequiredArgsStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclCheckRequiredArgsIdx].paramDefs, 
                     method_definitions[XOTclCheckRequiredArgsIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    char *name = (char *)pc.clientData[0];
    Tcl_Obj *value = (Tcl_Obj *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclCheckRequiredArgs(interp, name, value);

  }
}

static int
XOTclCAllocMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclClass *cl =  XOTclObjectToClass(clientData);
  if (!cl) return XOTclObjErrType(interp, objv[0], "Class");
  if (ArgumentParse(interp, objc, objv, (XOTclObject *) cl, objv[0], 
                     method_definitions[XOTclCAllocMethodIdx].paramDefs, 
                     method_definitions[XOTclCAllocMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    Tcl_Obj *name = (Tcl_Obj *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclCAllocMethod(interp, cl, name);

  }
}

static int
XOTclCCreateMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclClass *cl =  XOTclObjectToClass(clientData);
  if (!cl) return XOTclObjErrType(interp, objv[0], "Class");
  if (ArgumentParse(interp, objc, objv, (XOTclObject *) cl, objv[0], 
                     method_definitions[XOTclCCreateMethodIdx].paramDefs, 
                     method_definitions[XOTclCCreateMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    char *name = (char *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclCCreateMethod(interp, cl, name, objc, objv);

  }
}

static int
XOTclCDeallocMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclClass *cl =  XOTclObjectToClass(clientData);
  if (!cl) return XOTclObjErrType(interp, objv[0], "Class");
  if (ArgumentParse(interp, objc, objv, (XOTclObject *) cl, objv[0], 
                     method_definitions[XOTclCDeallocMethodIdx].paramDefs, 
                     method_definitions[XOTclCDeallocMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    Tcl_Obj *object = (Tcl_Obj *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclCDeallocMethod(interp, cl, object);

  }
}

static int
XOTclCForwardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclClass *cl =  XOTclObjectToClass(clientData);
  if (!cl) return XOTclObjErrType(interp, objv[0], "Class");
  if (ArgumentParse(interp, objc, objv, (XOTclObject *) cl, objv[0], 
                     method_definitions[XOTclCForwardMethodIdx].paramDefs, 
                     method_definitions[XOTclCForwardMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    int withPer_object = (int )pc.clientData[0];
    Tcl_Obj *name = (Tcl_Obj *)pc.clientData[1];
    Tcl_Obj *withDefault = (Tcl_Obj *)pc.clientData[2];
    int withEarlybinding = (int )pc.clientData[3];
    Tcl_Obj *withMethodprefix = (Tcl_Obj *)pc.clientData[4];
    int withObjscope = (int )pc.clientData[5];
    Tcl_Obj *withOnerror = (Tcl_Obj *)pc.clientData[6];
    int withVerbose = (int )pc.clientData[7];
    Tcl_Obj *target = (Tcl_Obj *)pc.clientData[8];

    parseContextRelease(&pc);
    return XOTclCForwardMethod(interp, cl, withPer_object, name, withDefault, withEarlybinding, withMethodprefix, withObjscope, withOnerror, withVerbose, target, objc-pc.lastobjc, objv+pc.lastobjc);

  }
}

static int
XOTclCInstFilterGuardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclClass *cl =  XOTclObjectToClass(clientData);
  if (!cl) return XOTclObjErrType(interp, objv[0], "Class");
  if (ArgumentParse(interp, objc, objv, (XOTclObject *) cl, objv[0], 
                     method_definitions[XOTclCInstFilterGuardMethodIdx].paramDefs, 
                     method_definitions[XOTclCInstFilterGuardMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    char *filter = (char *)pc.clientData[0];
    Tcl_Obj *guard = (Tcl_Obj *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclCInstFilterGuardMethod(interp, cl, filter, guard);

  }
}

static int
XOTclCInvalidateObjectParameterMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclClass *cl =  XOTclObjectToClass(clientData);
  if (!cl) return XOTclObjErrType(interp, objv[0], "Class");
  if (ArgumentParse(interp, objc, objv, (XOTclObject *) cl, objv[0], 
                     method_definitions[XOTclCInvalidateObjectParameterMethodIdx].paramDefs, 
                     method_definitions[XOTclCInvalidateObjectParameterMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    

    parseContextRelease(&pc);
    return XOTclCInvalidateObjectParameterMethod(interp, cl);

  }
}

static int
XOTclCInvariantsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclClass *cl =  XOTclObjectToClass(clientData);
  if (!cl) return XOTclObjErrType(interp, objv[0], "Class");
  if (ArgumentParse(interp, objc, objv, (XOTclObject *) cl, objv[0], 
                     method_definitions[XOTclCInvariantsMethodIdx].paramDefs, 
                     method_definitions[XOTclCInvariantsMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    Tcl_Obj *invariantlist = (Tcl_Obj *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclCInvariantsMethod(interp, cl, invariantlist);

  }
}

static int
XOTclCMethodMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclClass *cl =  XOTclObjectToClass(clientData);
  if (!cl) return XOTclObjErrType(interp, objv[0], "Class");
  if (ArgumentParse(interp, objc, objv, (XOTclObject *) cl, objv[0], 
                     method_definitions[XOTclCMethodMethodIdx].paramDefs, 
                     method_definitions[XOTclCMethodMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    int withInner_namespace = (int )pc.clientData[0];
    int withPer_object = (int )pc.clientData[1];
    int withProtected = (int )pc.clientData[2];
    Tcl_Obj *name = (Tcl_Obj *)pc.clientData[3];
    Tcl_Obj *args = (Tcl_Obj *)pc.clientData[4];
    Tcl_Obj *body = (Tcl_Obj *)pc.clientData[5];
    Tcl_Obj *withPrecondition = (Tcl_Obj *)pc.clientData[6];
    Tcl_Obj *withPostcondition = (Tcl_Obj *)pc.clientData[7];

    parseContextRelease(&pc);
    return XOTclCMethodMethod(interp, cl, withInner_namespace, withPer_object, withProtected, name, args, body, withPrecondition, withPostcondition);

  }
}

static int
XOTclCMixinGuardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclClass *cl =  XOTclObjectToClass(clientData);
  if (!cl) return XOTclObjErrType(interp, objv[0], "Class");
  if (ArgumentParse(interp, objc, objv, (XOTclObject *) cl, objv[0], 
                     method_definitions[XOTclCMixinGuardMethodIdx].paramDefs, 
                     method_definitions[XOTclCMixinGuardMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    int withPer_object = (int )pc.clientData[0];
    char *mixin = (char *)pc.clientData[1];
    Tcl_Obj *guard = (Tcl_Obj *)pc.clientData[2];

    parseContextRelease(&pc);
    return XOTclCMixinGuardMethod(interp, cl, withPer_object, mixin, guard);

  }
}

static int
XOTclCNewMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclClass *cl =  XOTclObjectToClass(clientData);
  if (!cl) return XOTclObjErrType(interp, objv[0], "Class");
  if (ArgumentParse(interp, objc, objv, (XOTclObject *) cl, objv[0], 
                     method_definitions[XOTclCNewMethodIdx].paramDefs, 
                     method_definitions[XOTclCNewMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *withChildof = (XOTclObject *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclCNewMethod(interp, cl, withChildof, objc-pc.lastobjc, objv+pc.lastobjc);

  }
}

static int
XOTclCRecreateMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclClass *cl =  XOTclObjectToClass(clientData);
  if (!cl) return XOTclObjErrType(interp, objv[0], "Class");
  if (ArgumentParse(interp, objc, objv, (XOTclObject *) cl, objv[0], 
                     method_definitions[XOTclCRecreateMethodIdx].paramDefs, 
                     method_definitions[XOTclCRecreateMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    Tcl_Obj *name = (Tcl_Obj *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclCRecreateMethod(interp, cl, name, objc, objv);

  }
}

static int
XOTclCSetterMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclClass *cl =  XOTclObjectToClass(clientData);
  if (!cl) return XOTclObjErrType(interp, objv[0], "Class");
  if (ArgumentParse(interp, objc, objv, (XOTclObject *) cl, objv[0], 
                     method_definitions[XOTclCSetterMethodIdx].paramDefs, 
                     method_definitions[XOTclCSetterMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    int withPer_object = (int )pc.clientData[0];
    char *name = (char *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclCSetterMethod(interp, cl, withPer_object, name);

  }
}

static int
XOTclClassInfoAliasMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclClassInfoAliasMethodIdx].paramDefs, 
                     method_definitions[XOTclClassInfoAliasMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclClass *object = (XOTclClass *)pc.clientData[0];
    int withDefinition = (int )pc.clientData[1];
    char *name = (char *)pc.clientData[2];

    parseContextRelease(&pc);
    return XOTclClassInfoAliasMethod(interp, object, withDefinition, name);

  }
}

static int
XOTclClassInfoFilterMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclClassInfoFilterMethodIdx].paramDefs, 
                     method_definitions[XOTclClassInfoFilterMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclClass *class = (XOTclClass *)pc.clientData[0];
    int withGuards = (int )pc.clientData[1];
    char *pattern = (char *)pc.clientData[2];

    parseContextRelease(&pc);
    return XOTclClassInfoFilterMethod(interp, class, withGuards, pattern);

  }
}

static int
XOTclClassInfoFilterguardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclClassInfoFilterguardMethodIdx].paramDefs, 
                     method_definitions[XOTclClassInfoFilterguardMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclClass *class = (XOTclClass *)pc.clientData[0];
    char *filter = (char *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclClassInfoFilterguardMethod(interp, class, filter);

  }
}

static int
XOTclClassInfoHeritageMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclClassInfoHeritageMethodIdx].paramDefs, 
                     method_definitions[XOTclClassInfoHeritageMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclClass *class = (XOTclClass *)pc.clientData[0];
    char *pattern = (char *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclClassInfoHeritageMethod(interp, class, pattern);

  }
}

static int
XOTclClassInfoInstancesMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclClassInfoInstancesMethodIdx].paramDefs, 
                     method_definitions[XOTclClassInfoInstancesMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclClass *class = (XOTclClass *)pc.clientData[0];
    int withClosure = (int )pc.clientData[1];
    char *patternString = NULL;
    XOTclObject *patternObj = NULL;
    Tcl_Obj *pattern = (Tcl_Obj *)pc.clientData[2];
    int returnCode;

    if (getMatchObject(interp, pattern,  objv[2], &patternObj, &patternString) == -1) {
      if (pattern) {
        DECR_REF_COUNT(pattern);
      }
      return TCL_OK;
    }
          
    parseContextRelease(&pc);
    returnCode = XOTclClassInfoInstancesMethod(interp, class, withClosure, patternString, patternObj);

    if (pattern) {
      DECR_REF_COUNT(pattern);
    }
    return returnCode;
  }
}

static int
XOTclClassInfoInstforwardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclClassInfoInstforwardMethodIdx].paramDefs, 
                     method_definitions[XOTclClassInfoInstforwardMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclClass *class = (XOTclClass *)pc.clientData[0];
    int withDefinition = (int )pc.clientData[1];
    char *name = (char *)pc.clientData[2];

    parseContextRelease(&pc);
    return XOTclClassInfoInstforwardMethod(interp, class, withDefinition, name);

  }
}

static int
XOTclClassInfoInstinvarMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclClassInfoInstinvarMethodIdx].paramDefs, 
                     method_definitions[XOTclClassInfoInstinvarMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclClass *class = (XOTclClass *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclClassInfoInstinvarMethod(interp, class);

  }
}

static int
XOTclClassInfoInstmixinofMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclClassInfoInstmixinofMethodIdx].paramDefs, 
                     method_definitions[XOTclClassInfoInstmixinofMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclClass *class = (XOTclClass *)pc.clientData[0];
    int withClosure = (int )pc.clientData[1];
    char *patternString = NULL;
    XOTclObject *patternObj = NULL;
    Tcl_Obj *pattern = (Tcl_Obj *)pc.clientData[2];
    int returnCode;

    if (getMatchObject(interp, pattern,  objv[2], &patternObj, &patternString) == -1) {
      if (pattern) {
        DECR_REF_COUNT(pattern);
      }
      return TCL_OK;
    }
          
    parseContextRelease(&pc);
    returnCode = XOTclClassInfoInstmixinofMethod(interp, class, withClosure, patternString, patternObj);

    if (pattern) {
      DECR_REF_COUNT(pattern);
    }
    return returnCode;
  }
}

static int
XOTclClassInfoInstparamsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclClassInfoInstparamsMethodIdx].paramDefs, 
                     method_definitions[XOTclClassInfoInstparamsMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclClass *class = (XOTclClass *)pc.clientData[0];
    char *methodName = (char *)pc.clientData[1];
    int withVarnames = (int )pc.clientData[2];

    parseContextRelease(&pc);
    return XOTclClassInfoInstparamsMethod(interp, class, methodName, withVarnames);

  }
}

static int
XOTclClassInfoInstpostMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclClassInfoInstpostMethodIdx].paramDefs, 
                     method_definitions[XOTclClassInfoInstpostMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclClass *class = (XOTclClass *)pc.clientData[0];
    char *methodName = (char *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclClassInfoInstpostMethod(interp, class, methodName);

  }
}

static int
XOTclClassInfoInstpreMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclClassInfoInstpreMethodIdx].paramDefs, 
                     method_definitions[XOTclClassInfoInstpreMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclClass *class = (XOTclClass *)pc.clientData[0];
    char *methodName = (char *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclClassInfoInstpreMethod(interp, class, methodName);

  }
}

static int
XOTclClassInfoMethodMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclClassInfoMethodMethodIdx].paramDefs, 
                     method_definitions[XOTclClassInfoMethodMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclClass *class = (XOTclClass *)pc.clientData[0];
    int infomethodsubcmd = (int )pc.clientData[1];
    char *name = (char *)pc.clientData[2];

    parseContextRelease(&pc);
    return XOTclClassInfoMethodMethod(interp, class, infomethodsubcmd, name);

  }
}

static int
XOTclClassInfoMethodsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclClassInfoMethodsMethodIdx].paramDefs, 
                     method_definitions[XOTclClassInfoMethodsMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclClass *object = (XOTclClass *)pc.clientData[0];
    int withDefined = (int )pc.clientData[1];
    int withMethodtype = (int )pc.clientData[2];
    int withNomixins = (int )pc.clientData[3];
    int withIncontext = (int )pc.clientData[4];
    char *pattern = (char *)pc.clientData[5];

    parseContextRelease(&pc);
    return XOTclClassInfoMethodsMethod(interp, object, withDefined, withMethodtype, withNomixins, withIncontext, pattern);

  }
}

static int
XOTclClassInfoMixinMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclClassInfoMixinMethodIdx].paramDefs, 
                     method_definitions[XOTclClassInfoMixinMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclClass *class = (XOTclClass *)pc.clientData[0];
    int withClosure = (int )pc.clientData[1];
    int withGuards = (int )pc.clientData[2];
    char *patternString = NULL;
    XOTclObject *patternObj = NULL;
    Tcl_Obj *pattern = (Tcl_Obj *)pc.clientData[3];
    int returnCode;

    if (getMatchObject(interp, pattern,  objv[3], &patternObj, &patternString) == -1) {
      if (pattern) {
        DECR_REF_COUNT(pattern);
      }
      return TCL_OK;
    }
          
    parseContextRelease(&pc);
    returnCode = XOTclClassInfoMixinMethod(interp, class, withClosure, withGuards, patternString, patternObj);

    if (pattern) {
      DECR_REF_COUNT(pattern);
    }
    return returnCode;
  }
}

static int
XOTclClassInfoMixinguardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclClassInfoMixinguardMethodIdx].paramDefs, 
                     method_definitions[XOTclClassInfoMixinguardMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclClass *class = (XOTclClass *)pc.clientData[0];
    char *mixin = (char *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclClassInfoMixinguardMethod(interp, class, mixin);

  }
}

static int
XOTclClassInfoMixinofMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclClassInfoMixinofMethodIdx].paramDefs, 
                     method_definitions[XOTclClassInfoMixinofMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclClass *class = (XOTclClass *)pc.clientData[0];
    int withClosure = (int )pc.clientData[1];
    char *patternString = NULL;
    XOTclObject *patternObj = NULL;
    Tcl_Obj *pattern = (Tcl_Obj *)pc.clientData[2];
    int returnCode;

    if (getMatchObject(interp, pattern,  objv[2], &patternObj, &patternString) == -1) {
      if (pattern) {
        DECR_REF_COUNT(pattern);
      }
      return TCL_OK;
    }
          
    parseContextRelease(&pc);
    returnCode = XOTclClassInfoMixinofMethod(interp, class, withClosure, patternString, patternObj);

    if (pattern) {
      DECR_REF_COUNT(pattern);
    }
    return returnCode;
  }
}

static int
XOTclClassInfoParameterMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclClassInfoParameterMethodIdx].paramDefs, 
                     method_definitions[XOTclClassInfoParameterMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclClass *class = (XOTclClass *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclClassInfoParameterMethod(interp, class);

  }
}

static int
XOTclClassInfoSlotsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclClassInfoSlotsMethodIdx].paramDefs, 
                     method_definitions[XOTclClassInfoSlotsMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclClass *class = (XOTclClass *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclClassInfoSlotsMethod(interp, class);

  }
}

static int
XOTclClassInfoSubclassMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclClassInfoSubclassMethodIdx].paramDefs, 
                     method_definitions[XOTclClassInfoSubclassMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclClass *class = (XOTclClass *)pc.clientData[0];
    int withClosure = (int )pc.clientData[1];
    char *patternString = NULL;
    XOTclObject *patternObj = NULL;
    Tcl_Obj *pattern = (Tcl_Obj *)pc.clientData[2];
    int returnCode;

    if (getMatchObject(interp, pattern,  objv[2], &patternObj, &patternString) == -1) {
      if (pattern) {
        DECR_REF_COUNT(pattern);
      }
      return TCL_OK;
    }
          
    parseContextRelease(&pc);
    returnCode = XOTclClassInfoSubclassMethod(interp, class, withClosure, patternString, patternObj);

    if (pattern) {
      DECR_REF_COUNT(pattern);
    }
    return returnCode;
  }
}

static int
XOTclClassInfoSuperclassMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclClassInfoSuperclassMethodIdx].paramDefs, 
                     method_definitions[XOTclClassInfoSuperclassMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclClass *class = (XOTclClass *)pc.clientData[0];
    int withClosure = (int )pc.clientData[1];
    Tcl_Obj *pattern = (Tcl_Obj *)pc.clientData[2];

    parseContextRelease(&pc);
    return XOTclClassInfoSuperclassMethod(interp, class, withClosure, pattern);

  }
}

static int
XOTclObjInfoAliasMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclObjInfoAliasMethodIdx].paramDefs, 
                     method_definitions[XOTclObjInfoAliasMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];
    int withDefinition = (int )pc.clientData[1];
    char *name = (char *)pc.clientData[2];

    parseContextRelease(&pc);
    return XOTclObjInfoAliasMethod(interp, object, withDefinition, name);

  }
}

static int
XOTclObjInfoCheckMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclObjInfoCheckMethodIdx].paramDefs, 
                     method_definitions[XOTclObjInfoCheckMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclObjInfoCheckMethod(interp, object);

  }
}

static int
XOTclObjInfoChildrenMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclObjInfoChildrenMethodIdx].paramDefs, 
                     method_definitions[XOTclObjInfoChildrenMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];
    char *pattern = (char *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclObjInfoChildrenMethod(interp, object, pattern);

  }
}

static int
XOTclObjInfoClassMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclObjInfoClassMethodIdx].paramDefs, 
                     method_definitions[XOTclObjInfoClassMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclObjInfoClassMethod(interp, object);

  }
}

static int
XOTclObjInfoFilterMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclObjInfoFilterMethodIdx].paramDefs, 
                     method_definitions[XOTclObjInfoFilterMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];
    int withOrder = (int )pc.clientData[1];
    int withGuards = (int )pc.clientData[2];
    char *pattern = (char *)pc.clientData[3];

    parseContextRelease(&pc);
    return XOTclObjInfoFilterMethod(interp, object, withOrder, withGuards, pattern);

  }
}

static int
XOTclObjInfoFilterguardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclObjInfoFilterguardMethodIdx].paramDefs, 
                     method_definitions[XOTclObjInfoFilterguardMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];
    char *filter = (char *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclObjInfoFilterguardMethod(interp, object, filter);

  }
}

static int
XOTclObjInfoForwardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclObjInfoForwardMethodIdx].paramDefs, 
                     method_definitions[XOTclObjInfoForwardMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];
    int withDefinition = (int )pc.clientData[1];
    char *name = (char *)pc.clientData[2];

    parseContextRelease(&pc);
    return XOTclObjInfoForwardMethod(interp, object, withDefinition, name);

  }
}

static int
XOTclObjInfoHasnamespaceMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclObjInfoHasnamespaceMethodIdx].paramDefs, 
                     method_definitions[XOTclObjInfoHasnamespaceMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclObjInfoHasnamespaceMethod(interp, object);

  }
}

static int
XOTclObjInfoInvarMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclObjInfoInvarMethodIdx].paramDefs, 
                     method_definitions[XOTclObjInfoInvarMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclObjInfoInvarMethod(interp, object);

  }
}

static int
XOTclObjInfoMethodMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclObjInfoMethodMethodIdx].paramDefs, 
                     method_definitions[XOTclObjInfoMethodMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];
    int infomethodsubcmd = (int )pc.clientData[1];
    char *name = (char *)pc.clientData[2];

    parseContextRelease(&pc);
    return XOTclObjInfoMethodMethod(interp, object, infomethodsubcmd, name);

  }
}

static int
XOTclObjInfoMethodsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclObjInfoMethodsMethodIdx].paramDefs, 
                     method_definitions[XOTclObjInfoMethodsMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];
    int withDefined = (int )pc.clientData[1];
    int withMethodtype = (int )pc.clientData[2];
    int withNomixins = (int )pc.clientData[3];
    int withIncontext = (int )pc.clientData[4];
    char *pattern = (char *)pc.clientData[5];

    parseContextRelease(&pc);
    return XOTclObjInfoMethodsMethod(interp, object, withDefined, withMethodtype, withNomixins, withIncontext, pattern);

  }
}

static int
XOTclObjInfoMixinMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclObjInfoMixinMethodIdx].paramDefs, 
                     method_definitions[XOTclObjInfoMixinMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];
    int withGuards = (int )pc.clientData[1];
    int withOrder = (int )pc.clientData[2];
    char *patternString = NULL;
    XOTclObject *patternObj = NULL;
    Tcl_Obj *pattern = (Tcl_Obj *)pc.clientData[3];
    int returnCode;

    if (getMatchObject(interp, pattern,  objv[3], &patternObj, &patternString) == -1) {
      if (pattern) {
        DECR_REF_COUNT(pattern);
      }
      return TCL_OK;
    }
          
    parseContextRelease(&pc);
    returnCode = XOTclObjInfoMixinMethod(interp, object, withGuards, withOrder, patternString, patternObj);

    if (pattern) {
      DECR_REF_COUNT(pattern);
    }
    return returnCode;
  }
}

static int
XOTclObjInfoMixinguardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclObjInfoMixinguardMethodIdx].paramDefs, 
                     method_definitions[XOTclObjInfoMixinguardMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];
    char *mixin = (char *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclObjInfoMixinguardMethod(interp, object, mixin);

  }
}

static int
XOTclObjInfoParamsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclObjInfoParamsMethodIdx].paramDefs, 
                     method_definitions[XOTclObjInfoParamsMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];
    char *methodName = (char *)pc.clientData[1];
    int withVarnames = (int )pc.clientData[2];

    parseContextRelease(&pc);
    return XOTclObjInfoParamsMethod(interp, object, methodName, withVarnames);

  }
}

static int
XOTclObjInfoParentMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclObjInfoParentMethodIdx].paramDefs, 
                     method_definitions[XOTclObjInfoParentMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclObjInfoParentMethod(interp, object);

  }
}

static int
XOTclObjInfoPostMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclObjInfoPostMethodIdx].paramDefs, 
                     method_definitions[XOTclObjInfoPostMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];
    char *methodName = (char *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclObjInfoPostMethod(interp, object, methodName);

  }
}

static int
XOTclObjInfoPreMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclObjInfoPreMethodIdx].paramDefs, 
                     method_definitions[XOTclObjInfoPreMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];
    char *methodName = (char *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclObjInfoPreMethod(interp, object, methodName);

  }
}

static int
XOTclObjInfoPrecedenceMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclObjInfoPrecedenceMethodIdx].paramDefs, 
                     method_definitions[XOTclObjInfoPrecedenceMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];
    int withIntrinsic = (int )pc.clientData[1];
    char *pattern = (char *)pc.clientData[2];

    parseContextRelease(&pc);
    return XOTclObjInfoPrecedenceMethod(interp, object, withIntrinsic, pattern);

  }
}

static int
XOTclObjInfoSlotObjectsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclObjInfoSlotObjectsMethodIdx].paramDefs, 
                     method_definitions[XOTclObjInfoSlotObjectsMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];
    char *pattern = (char *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclObjInfoSlotObjectsMethod(interp, object, pattern);

  }
}

static int
XOTclObjInfoVarsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclObjInfoVarsMethodIdx].paramDefs, 
                     method_definitions[XOTclObjInfoVarsMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];
    char *pattern = (char *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclObjInfoVarsMethod(interp, object, pattern);

  }
}

static int
XOTclOAutonameMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
  if (ArgumentParse(interp, objc, objv, obj, objv[0], 
                     method_definitions[XOTclOAutonameMethodIdx].paramDefs, 
                     method_definitions[XOTclOAutonameMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    int withInstance = (int )pc.clientData[0];
    int withReset = (int )pc.clientData[1];
    Tcl_Obj *name = (Tcl_Obj *)pc.clientData[2];

    parseContextRelease(&pc);
    return XOTclOAutonameMethod(interp, obj, withInstance, withReset, name);

  }
}

static int
XOTclOCheckMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
  if (ArgumentParse(interp, objc, objv, obj, objv[0], 
                     method_definitions[XOTclOCheckMethodIdx].paramDefs, 
                     method_definitions[XOTclOCheckMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    Tcl_Obj *flag = (Tcl_Obj *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclOCheckMethod(interp, obj, flag);

  }
}

static int
XOTclOCleanupMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
  if (ArgumentParse(interp, objc, objv, obj, objv[0], 
                     method_definitions[XOTclOCleanupMethodIdx].paramDefs, 
                     method_definitions[XOTclOCleanupMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    

    parseContextRelease(&pc);
    return XOTclOCleanupMethod(interp, obj);

  }
}

static int
XOTclOConfigureMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
    

    return XOTclOConfigureMethod(interp, obj, objc, objv);

}

static int
XOTclODestroyMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
  if (ArgumentParse(interp, objc, objv, obj, objv[0], 
                     method_definitions[XOTclODestroyMethodIdx].paramDefs, 
                     method_definitions[XOTclODestroyMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    

    parseContextRelease(&pc);
    return XOTclODestroyMethod(interp, obj);

  }
}

static int
XOTclOExistsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
  if (ArgumentParse(interp, objc, objv, obj, objv[0], 
                     method_definitions[XOTclOExistsMethodIdx].paramDefs, 
                     method_definitions[XOTclOExistsMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    char *var = (char *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclOExistsMethod(interp, obj, var);

  }
}

static int
XOTclOFilterGuardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
  if (ArgumentParse(interp, objc, objv, obj, objv[0], 
                     method_definitions[XOTclOFilterGuardMethodIdx].paramDefs, 
                     method_definitions[XOTclOFilterGuardMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    char *filter = (char *)pc.clientData[0];
    Tcl_Obj *guard = (Tcl_Obj *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclOFilterGuardMethod(interp, obj, filter, guard);

  }
}

static int
XOTclOFilterSearchMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
  if (ArgumentParse(interp, objc, objv, obj, objv[0], 
                     method_definitions[XOTclOFilterSearchMethodIdx].paramDefs, 
                     method_definitions[XOTclOFilterSearchMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    char *filter = (char *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclOFilterSearchMethod(interp, obj, filter);

  }
}

static int
XOTclOForwardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
  if (ArgumentParse(interp, objc, objv, obj, objv[0], 
                     method_definitions[XOTclOForwardMethodIdx].paramDefs, 
                     method_definitions[XOTclOForwardMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    Tcl_Obj *method = (Tcl_Obj *)pc.clientData[0];
    Tcl_Obj *withDefault = (Tcl_Obj *)pc.clientData[1];
    int withEarlybinding = (int )pc.clientData[2];
    Tcl_Obj *withMethodprefix = (Tcl_Obj *)pc.clientData[3];
    int withObjscope = (int )pc.clientData[4];
    Tcl_Obj *withOnerror = (Tcl_Obj *)pc.clientData[5];
    int withVerbose = (int )pc.clientData[6];
    Tcl_Obj *target = (Tcl_Obj *)pc.clientData[7];

    parseContextRelease(&pc);
    return XOTclOForwardMethod(interp, obj, method, withDefault, withEarlybinding, withMethodprefix, withObjscope, withOnerror, withVerbose, target, objc-pc.lastobjc, objv+pc.lastobjc);

  }
}

static int
XOTclOInstVarMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
    

    return XOTclOInstVarMethod(interp, obj, objc, objv);

}

static int
XOTclOInvariantsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
  if (ArgumentParse(interp, objc, objv, obj, objv[0], 
                     method_definitions[XOTclOInvariantsMethodIdx].paramDefs, 
                     method_definitions[XOTclOInvariantsMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    Tcl_Obj *invariantlist = (Tcl_Obj *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclOInvariantsMethod(interp, obj, invariantlist);

  }
}

static int
XOTclOMethodMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
  if (ArgumentParse(interp, objc, objv, obj, objv[0], 
                     method_definitions[XOTclOMethodMethodIdx].paramDefs, 
                     method_definitions[XOTclOMethodMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    int withInner_namespace = (int )pc.clientData[0];
    int withProtected = (int )pc.clientData[1];
    Tcl_Obj *name = (Tcl_Obj *)pc.clientData[2];
    Tcl_Obj *args = (Tcl_Obj *)pc.clientData[3];
    Tcl_Obj *body = (Tcl_Obj *)pc.clientData[4];
    Tcl_Obj *withPrecondition = (Tcl_Obj *)pc.clientData[5];
    Tcl_Obj *withPostcondition = (Tcl_Obj *)pc.clientData[6];

    parseContextRelease(&pc);
    return XOTclOMethodMethod(interp, obj, withInner_namespace, withProtected, name, args, body, withPrecondition, withPostcondition);

  }
}

static int
XOTclOMixinGuardMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
  if (ArgumentParse(interp, objc, objv, obj, objv[0], 
                     method_definitions[XOTclOMixinGuardMethodIdx].paramDefs, 
                     method_definitions[XOTclOMixinGuardMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    char *mixin = (char *)pc.clientData[0];
    Tcl_Obj *guard = (Tcl_Obj *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclOMixinGuardMethod(interp, obj, mixin, guard);

  }
}

static int
XOTclONextMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
    

    return XOTclONextMethod(interp, obj, objc, objv);

}

static int
XOTclONoinitMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
  if (ArgumentParse(interp, objc, objv, obj, objv[0], 
                     method_definitions[XOTclONoinitMethodIdx].paramDefs, 
                     method_definitions[XOTclONoinitMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    

    parseContextRelease(&pc);
    return XOTclONoinitMethod(interp, obj);

  }
}

static int
XOTclOProcSearchMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
  if (ArgumentParse(interp, objc, objv, obj, objv[0], 
                     method_definitions[XOTclOProcSearchMethodIdx].paramDefs, 
                     method_definitions[XOTclOProcSearchMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    char *name = (char *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclOProcSearchMethod(interp, obj, name);

  }
}

static int
XOTclORequireNamespaceMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
  if (ArgumentParse(interp, objc, objv, obj, objv[0], 
                     method_definitions[XOTclORequireNamespaceMethodIdx].paramDefs, 
                     method_definitions[XOTclORequireNamespaceMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    

    parseContextRelease(&pc);
    return XOTclORequireNamespaceMethod(interp, obj);

  }
}

static int
XOTclOResidualargsMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
    

    return XOTclOResidualargsMethod(interp, obj, objc, objv);

}

static int
XOTclOSetterMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
  if (ArgumentParse(interp, objc, objv, obj, objv[0], 
                     method_definitions[XOTclOSetterMethodIdx].paramDefs, 
                     method_definitions[XOTclOSetterMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    char *name = (char *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclOSetterMethod(interp, obj, name);

  }
}

static int
XOTclOUplevelMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
    

    return XOTclOUplevelMethod(interp, obj, objc, objv);

}

static int
XOTclOUpvarMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
    

    return XOTclOUpvarMethod(interp, obj, objc, objv);

}

static int
XOTclOVolatileMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
  if (ArgumentParse(interp, objc, objv, obj, objv[0], 
                     method_definitions[XOTclOVolatileMethodIdx].paramDefs, 
                     method_definitions[XOTclOVolatileMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    

    parseContextRelease(&pc);
    return XOTclOVolatileMethod(interp, obj);

  }
}

static int
XOTclOVwaitMethodStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;
  XOTclObject *obj =  (XOTclObject *)clientData;
  if (!obj) return XOTclObjErrType(interp, objv[0], "Object");
  if (ArgumentParse(interp, objc, objv, obj, objv[0], 
                     method_definitions[XOTclOVwaitMethodIdx].paramDefs, 
                     method_definitions[XOTclOVwaitMethodIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    char *varname = (char *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclOVwaitMethod(interp, obj, varname);

  }
}

static int
XOTclAliasCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclAliasCmdIdx].paramDefs, 
                     method_definitions[XOTclAliasCmdIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];
    char *methodName = (char *)pc.clientData[1];
    int withObjscope = (int )pc.clientData[2];
    int withPer_object = (int )pc.clientData[3];
    int withProtected = (int )pc.clientData[4];
    Tcl_Obj *cmdName = (Tcl_Obj *)pc.clientData[5];

    parseContextRelease(&pc);
    return XOTclAliasCmd(interp, object, methodName, withObjscope, withPer_object, withProtected, cmdName);

  }
}

static int
XOTclConfigureCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclConfigureCmdIdx].paramDefs, 
                     method_definitions[XOTclConfigureCmdIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    int configureoption = (int )pc.clientData[0];
    Tcl_Obj *value = (Tcl_Obj *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclConfigureCmd(interp, configureoption, value);

  }
}

static int
XOTclCreateObjectSystemCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclCreateObjectSystemCmdIdx].paramDefs, 
                     method_definitions[XOTclCreateObjectSystemCmdIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    Tcl_Obj *rootClass = (Tcl_Obj *)pc.clientData[0];
    Tcl_Obj *rootMetaClass = (Tcl_Obj *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclCreateObjectSystemCmd(interp, rootClass, rootMetaClass);

  }
}

static int
XOTclDeprecatedCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclDeprecatedCmdIdx].paramDefs, 
                     method_definitions[XOTclDeprecatedCmdIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    char *what = (char *)pc.clientData[0];
    char *oldCmd = (char *)pc.clientData[1];
    char *newCmd = (char *)pc.clientData[2];

    parseContextRelease(&pc);
    return XOTclDeprecatedCmd(interp, what, oldCmd, newCmd);

  }
}

static int
XOTclDispatchCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclDispatchCmdIdx].paramDefs, 
                     method_definitions[XOTclDispatchCmdIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];
    int withObjscope = (int )pc.clientData[1];
    Tcl_Obj *command = (Tcl_Obj *)pc.clientData[2];

    parseContextRelease(&pc);
    return XOTclDispatchCmd(interp, object, withObjscope, command, objc-pc.lastobjc, objv+pc.lastobjc);

  }
}

static int
XOTclDotCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {

    

    return XOTclDotCmd(interp, objc, objv);

}

static int
XOTclFinalizeObjCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclFinalizeObjCmdIdx].paramDefs, 
                     method_definitions[XOTclFinalizeObjCmdIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    

    parseContextRelease(&pc);
    return XOTclFinalizeObjCmd(interp);

  }
}

static int
XOTclGetSelfObjCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclGetSelfObjCmdIdx].paramDefs, 
                     method_definitions[XOTclGetSelfObjCmdIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    int selfoption = (int )pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclGetSelfObjCmd(interp, selfoption);

  }
}

static int
XOTclInstvarCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclInstvarCmdIdx].paramDefs, 
                     method_definitions[XOTclInstvarCmdIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *withObject = (XOTclObject *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclInstvarCmd(interp, withObject, objc-pc.lastobjc, objv+pc.lastobjc);

  }
}

static int
XOTclInterpObjCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclInterpObjCmdIdx].paramDefs, 
                     method_definitions[XOTclInterpObjCmdIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    char *name = (char *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclInterpObjCmd(interp, name, objc, objv);

  }
}

static int
XOTclIsCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclIsCmdIdx].paramDefs, 
                     method_definitions[XOTclIsCmdIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    Tcl_Obj *object = (Tcl_Obj *)pc.clientData[0];
    int objectkind = (int )pc.clientData[1];
    Tcl_Obj *value = (Tcl_Obj *)pc.clientData[2];

    parseContextRelease(&pc);
    return XOTclIsCmd(interp, object, objectkind, value);

  }
}

static int
XOTclMethodPropertyCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclMethodPropertyCmdIdx].paramDefs, 
                     method_definitions[XOTclMethodPropertyCmdIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];
    char *methodName = (char *)pc.clientData[1];
    int withPer_object = (int )pc.clientData[2];
    int methodproperty = (int )pc.clientData[3];
    Tcl_Obj *value = (Tcl_Obj *)pc.clientData[4];

    parseContextRelease(&pc);
    return XOTclMethodPropertyCmd(interp, object, methodName, withPer_object, methodproperty, value);

  }
}

static int
XOTclMyCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclMyCmdIdx].paramDefs, 
                     method_definitions[XOTclMyCmdIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    int withLocal = (int )pc.clientData[0];
    Tcl_Obj *method = (Tcl_Obj *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclMyCmd(interp, withLocal, method, objc-pc.lastobjc, objv+pc.lastobjc);

  }
}

static int
XOTclNSCopyCmdsStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclNSCopyCmdsIdx].paramDefs, 
                     method_definitions[XOTclNSCopyCmdsIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    Tcl_Obj *fromNs = (Tcl_Obj *)pc.clientData[0];
    Tcl_Obj *toNs = (Tcl_Obj *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclNSCopyCmds(interp, fromNs, toNs);

  }
}

static int
XOTclNSCopyVarsStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclNSCopyVarsIdx].paramDefs, 
                     method_definitions[XOTclNSCopyVarsIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    Tcl_Obj *fromNs = (Tcl_Obj *)pc.clientData[0];
    Tcl_Obj *toNs = (Tcl_Obj *)pc.clientData[1];

    parseContextRelease(&pc);
    return XOTclNSCopyVars(interp, fromNs, toNs);

  }
}

static int
XOTclQualifyObjCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclQualifyObjCmdIdx].paramDefs, 
                     method_definitions[XOTclQualifyObjCmdIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    Tcl_Obj *name = (Tcl_Obj *)pc.clientData[0];

    parseContextRelease(&pc);
    return XOTclQualifyObjCmd(interp, name);

  }
}

static int
XOTclRelationCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclRelationCmdIdx].paramDefs, 
                     method_definitions[XOTclRelationCmdIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];
    int withPer_object = (int )pc.clientData[1];
    int relationtype = (int )pc.clientData[2];
    Tcl_Obj *value = (Tcl_Obj *)pc.clientData[3];

    parseContextRelease(&pc);
    return XOTclRelationCmd(interp, object, withPer_object, relationtype, value);

  }
}

static int
XOTclSetInstvarCmdStub(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  parseContext pc;

  if (ArgumentParse(interp, objc, objv, NULL, objv[0], 
                     method_definitions[XOTclSetInstvarCmdIdx].paramDefs, 
                     method_definitions[XOTclSetInstvarCmdIdx].nrParameters, 
                     &pc) != TCL_OK) {
    return TCL_ERROR;
  } else {
    XOTclObject *object = (XOTclObject *)pc.clientData[0];
    Tcl_Obj *variable = (Tcl_Obj *)pc.clientData[1];
    Tcl_Obj *value = (Tcl_Obj *)pc.clientData[2];

    parseContextRelease(&pc);
    return XOTclSetInstvarCmd(interp, object, variable, value);

  }
}

static methodDefinition method_definitions[] = {
{"::xotcl::cmd::ParameterType::type=boolean", XOTclCheckBooleanArgsStub, 2, {
  {"name", 1, 0, convertToString},
  {"value", 0, 0, convertToTclobj}}
},
{"::xotcl::cmd::ParameterType::type=required", XOTclCheckRequiredArgsStub, 2, {
  {"name", 1, 0, convertToString},
  {"value", 0, 0, convertToTclobj}}
},
{"::xotcl::cmd::Class::alloc", XOTclCAllocMethodStub, 1, {
  {"name", 1, 0, convertToTclobj}}
},
{"::xotcl::cmd::Class::create", XOTclCCreateMethodStub, 2, {
  {"name", 1, 0, convertToString},
  {"args", 0, 0, convertToNothing}}
},
{"::xotcl::cmd::Class::dealloc", XOTclCDeallocMethodStub, 1, {
  {"object", 1, 0, convertToTclobj}}
},
{"::xotcl::cmd::Class::forward", XOTclCForwardMethodStub, 10, {
  {"-per-object", 0, 0, convertToBoolean},
  {"name", 1, 0, convertToTclobj},
  {"-default", 0, 1, convertToTclobj},
  {"-earlybinding", 0, 0, convertToString},
  {"-methodprefix", 0, 1, convertToTclobj},
  {"-objscope", 0, 0, convertToString},
  {"-onerror", 0, 1, convertToTclobj},
  {"-verbose", 0, 0, convertToString},
  {"target", 0, 0, convertToTclobj},
  {"args", 0, 0, convertToNothing}}
},
{"::xotcl::cmd::Class::instfilterguard", XOTclCInstFilterGuardMethodStub, 2, {
  {"filter", 1, 0, convertToString},
  {"guard", 1, 0, convertToTclobj}}
},
{"::xotcl::cmd::Class::invalidateobjectparameter", XOTclCInvalidateObjectParameterMethodStub, 0, {
  }
},
{"::xotcl::cmd::Class::instinvar", XOTclCInvariantsMethodStub, 1, {
  {"invariantlist", 1, 0, convertToTclobj}}
},
{"::xotcl::cmd::Class::method", XOTclCMethodMethodStub, 8, {
  {"-inner-namespace", 0, 0, convertToBoolean},
  {"-per-object", 0, 0, convertToBoolean},
  {"-protected", 0, 0, convertToString},
  {"name", 1, 0, convertToTclobj},
  {"args", 1, 0, convertToTclobj},
  {"body", 1, 0, convertToTclobj},
  {"-precondition", 0, 1, convertToTclobj},
  {"-postcondition", 0, 1, convertToTclobj}}
},
{"::xotcl::cmd::Class::mixinguard", XOTclCMixinGuardMethodStub, 3, {
  {"-per-object", 0, 0, convertToBoolean},
  {"mixin", 1, 0, convertToString},
  {"guard", 1, 0, convertToTclobj}}
},
{"::xotcl::cmd::Class::new", XOTclCNewMethodStub, 2, {
  {"-childof", 0, 1, convertToObject},
  {"args", 0, 0, convertToNothing}}
},
{"::xotcl::cmd::Class::recreate", XOTclCRecreateMethodStub, 2, {
  {"name", 1, 0, convertToTclobj},
  {"args", 0, 0, convertToNothing}}
},
{"::xotcl::cmd::Class::setter", XOTclCSetterMethodStub, 2, {
  {"-per-object", 0, 0, convertToBoolean},
  {"name", 1, 0, convertToString}}
},
{"::xotcl::cmd::ClassInfo::alias", XOTclClassInfoAliasMethodStub, 3, {
  {"object", 1, 0, convertToClass},
  {"-definition", 0, 0, convertToString},
  {"name", 0, 0, convertToString}}
},
{"::xotcl::cmd::ClassInfo::filter", XOTclClassInfoFilterMethodStub, 3, {
  {"class", 1, 0, convertToClass},
  {"-guards", 0, 0, convertToString},
  {"pattern", 0, 0, convertToString}}
},
{"::xotcl::cmd::ClassInfo::filterguard", XOTclClassInfoFilterguardMethodStub, 2, {
  {"class", 1, 0, convertToClass},
  {"filter", 1, 0, convertToString}}
},
{"::xotcl::cmd::ClassInfo::heritage", XOTclClassInfoHeritageMethodStub, 2, {
  {"class", 1, 0, convertToClass},
  {"pattern", 0, 0, convertToString}}
},
{"::xotcl::cmd::ClassInfo::instances", XOTclClassInfoInstancesMethodStub, 3, {
  {"class", 1, 0, convertToClass},
  {"-closure", 0, 0, convertToString},
  {"pattern", 0, 0, convertToObjpattern}}
},
{"::xotcl::cmd::ClassInfo::instforward", XOTclClassInfoInstforwardMethodStub, 3, {
  {"class", 1, 0, convertToClass},
  {"-definition", 0, 0, convertToString},
  {"name", 0, 0, convertToString}}
},
{"::xotcl::cmd::ClassInfo::instinvar", XOTclClassInfoInstinvarMethodStub, 1, {
  {"class", 1, 0, convertToClass}}
},
{"::xotcl::cmd::ClassInfo::instmixinof", XOTclClassInfoInstmixinofMethodStub, 3, {
  {"class", 1, 0, convertToClass},
  {"-closure", 0, 0, convertToString},
  {"pattern", 0, 0, convertToObjpattern}}
},
{"::xotcl::cmd::ClassInfo::instparams", XOTclClassInfoInstparamsMethodStub, 3, {
  {"class", 1, 0, convertToClass},
  {"methodName", 1, 0, convertToString},
  {"-varNames", 0, 0, convertToString}}
},
{"::xotcl::cmd::ClassInfo::instpost", XOTclClassInfoInstpostMethodStub, 2, {
  {"class", 1, 0, convertToClass},
  {"methodName", 1, 0, convertToString}}
},
{"::xotcl::cmd::ClassInfo::instpre", XOTclClassInfoInstpreMethodStub, 2, {
  {"class", 1, 0, convertToClass},
  {"methodName", 1, 0, convertToString}}
},
{"::xotcl::cmd::ClassInfo::method", XOTclClassInfoMethodMethodStub, 3, {
  {"class", 0, 0, convertToClass},
  {"infomethodsubcmd", 0, 0, convertToInfomethodsubcmd},
  {"name", 0, 0, convertToString}}
},
{"::xotcl::cmd::ClassInfo::methods", XOTclClassInfoMethodsMethodStub, 6, {
  {"object", 0, 0, convertToClass},
  {"-defined", 0, 0, convertToString},
  {"-methodtype", 0, 1, convertToMethodtype},
  {"-nomixins", 0, 0, convertToString},
  {"-incontext", 0, 0, convertToString},
  {"pattern", 0, 0, convertToString}}
},
{"::xotcl::cmd::ClassInfo::mixin", XOTclClassInfoMixinMethodStub, 4, {
  {"class", 1, 0, convertToClass},
  {"-closure", 0, 0, convertToString},
  {"-guards", 0, 0, convertToString},
  {"pattern", 0, 0, convertToObjpattern}}
},
{"::xotcl::cmd::ClassInfo::mixinguard", XOTclClassInfoMixinguardMethodStub, 2, {
  {"class", 1, 0, convertToClass},
  {"mixin", 1, 0, convertToString}}
},
{"::xotcl::cmd::ClassInfo::mixinof", XOTclClassInfoMixinofMethodStub, 3, {
  {"class", 1, 0, convertToClass},
  {"-closure", 0, 0, convertToString},
  {"pattern", 0, 0, convertToObjpattern}}
},
{"::xotcl::cmd::ClassInfo::parameter", XOTclClassInfoParameterMethodStub, 1, {
  {"class", 1, 0, convertToClass}}
},
{"::xotcl::cmd::ClassInfo::slots", XOTclClassInfoSlotsMethodStub, 1, {
  {"class", 1, 0, convertToClass}}
},
{"::xotcl::cmd::ClassInfo::subclass", XOTclClassInfoSubclassMethodStub, 3, {
  {"class", 1, 0, convertToClass},
  {"-closure", 0, 0, convertToString},
  {"pattern", 0, 0, convertToObjpattern}}
},
{"::xotcl::cmd::ClassInfo::superclass", XOTclClassInfoSuperclassMethodStub, 3, {
  {"class", 1, 0, convertToClass},
  {"-closure", 0, 0, convertToString},
  {"pattern", 0, 0, convertToTclobj}}
},
{"::xotcl::cmd::ObjectInfo::alias", XOTclObjInfoAliasMethodStub, 3, {
  {"object", 1, 0, convertToObject},
  {"-definition", 0, 0, convertToString},
  {"name", 0, 0, convertToString}}
},
{"::xotcl::cmd::ObjectInfo::check", XOTclObjInfoCheckMethodStub, 1, {
  {"object", 1, 0, convertToObject}}
},
{"::xotcl::cmd::ObjectInfo::children", XOTclObjInfoChildrenMethodStub, 2, {
  {"object", 1, 0, convertToObject},
  {"pattern", 0, 0, convertToString}}
},
{"::xotcl::cmd::ObjectInfo::class", XOTclObjInfoClassMethodStub, 1, {
  {"object", 1, 0, convertToObject}}
},
{"::xotcl::cmd::ObjectInfo::filter", XOTclObjInfoFilterMethodStub, 4, {
  {"object", 1, 0, convertToObject},
  {"-order", 0, 0, convertToString},
  {"-guards", 0, 0, convertToString},
  {"pattern", 0, 0, convertToString}}
},
{"::xotcl::cmd::ObjectInfo::filterguard", XOTclObjInfoFilterguardMethodStub, 2, {
  {"object", 1, 0, convertToObject},
  {"filter", 1, 0, convertToString}}
},
{"::xotcl::cmd::ObjectInfo::forward", XOTclObjInfoForwardMethodStub, 3, {
  {"object", 1, 0, convertToObject},
  {"-definition", 0, 0, convertToString},
  {"name", 0, 0, convertToString}}
},
{"::xotcl::cmd::ObjectInfo::hasnamespace", XOTclObjInfoHasnamespaceMethodStub, 1, {
  {"object", 1, 0, convertToObject}}
},
{"::xotcl::cmd::ObjectInfo::invar", XOTclObjInfoInvarMethodStub, 1, {
  {"object", 1, 0, convertToObject}}
},
{"::xotcl::cmd::ObjectInfo::method", XOTclObjInfoMethodMethodStub, 3, {
  {"object", 0, 0, convertToObject},
  {"infomethodsubcmd", 0, 0, convertToInfomethodsubcmd},
  {"name", 0, 0, convertToString}}
},
{"::xotcl::cmd::ObjectInfo::methods", XOTclObjInfoMethodsMethodStub, 6, {
  {"object", 0, 0, convertToObject},
  {"-defined", 0, 0, convertToString},
  {"-methodtype", 0, 1, convertToMethodtype},
  {"-nomixins", 0, 0, convertToString},
  {"-incontext", 0, 0, convertToString},
  {"pattern", 0, 0, convertToString}}
},
{"::xotcl::cmd::ObjectInfo::mixin", XOTclObjInfoMixinMethodStub, 4, {
  {"object", 1, 0, convertToObject},
  {"-guards", 0, 0, convertToString},
  {"-order", 0, 0, convertToString},
  {"pattern", 0, 0, convertToObjpattern}}
},
{"::xotcl::cmd::ObjectInfo::mixinguard", XOTclObjInfoMixinguardMethodStub, 2, {
  {"object", 1, 0, convertToObject},
  {"mixin", 1, 0, convertToString}}
},
{"::xotcl::cmd::ObjectInfo::params", XOTclObjInfoParamsMethodStub, 3, {
  {"object", 1, 0, convertToObject},
  {"methodName", 1, 0, convertToString},
  {"-varNames", 0, 0, convertToString}}
},
{"::xotcl::cmd::ObjectInfo::parent", XOTclObjInfoParentMethodStub, 1, {
  {"object", 1, 0, convertToObject}}
},
{"::xotcl::cmd::ObjectInfo::post", XOTclObjInfoPostMethodStub, 2, {
  {"object", 1, 0, convertToObject},
  {"methodName", 1, 0, convertToString}}
},
{"::xotcl::cmd::ObjectInfo::pre", XOTclObjInfoPreMethodStub, 2, {
  {"object", 1, 0, convertToObject},
  {"methodName", 1, 0, convertToString}}
},
{"::xotcl::cmd::ObjectInfo::precedence", XOTclObjInfoPrecedenceMethodStub, 3, {
  {"object", 1, 0, convertToObject},
  {"-intrinsic", 0, 0, convertToString},
  {"pattern", 0, 0, convertToString}}
},
{"::xotcl::cmd::ObjectInfo::slotobjects", XOTclObjInfoSlotObjectsMethodStub, 2, {
  {"object", 1, 0, convertToObject},
  {"pattern", 0, 0, convertToString}}
},
{"::xotcl::cmd::ObjectInfo::vars", XOTclObjInfoVarsMethodStub, 2, {
  {"object", 1, 0, convertToObject},
  {"pattern", 0, 0, convertToString}}
},
{"::xotcl::cmd::Object::autoname", XOTclOAutonameMethodStub, 3, {
  {"-instance", 0, 0, convertToString},
  {"-reset", 0, 0, convertToString},
  {"name", 1, 0, convertToTclobj}}
},
{"::xotcl::cmd::Object::check", XOTclOCheckMethodStub, 1, {
  {"flag", 1, 0, convertToTclobj}}
},
{"::xotcl::cmd::Object::cleanup", XOTclOCleanupMethodStub, 0, {
  }
},
{"::xotcl::cmd::Object::configure", XOTclOConfigureMethodStub, 1, {
  {"args", 0, 0, convertToNothing}}
},
{"::xotcl::cmd::Object::destroy", XOTclODestroyMethodStub, 0, {
  }
},
{"::xotcl::cmd::Object::exists", XOTclOExistsMethodStub, 1, {
  {"var", 1, 0, convertToString}}
},
{"::xotcl::cmd::Object::filterguard", XOTclOFilterGuardMethodStub, 2, {
  {"filter", 1, 0, convertToString},
  {"guard", 1, 0, convertToTclobj}}
},
{"::xotcl::cmd::Object::filtersearch", XOTclOFilterSearchMethodStub, 1, {
  {"filter", 1, 0, convertToString}}
},
{"::xotcl::cmd::Object::forward", XOTclOForwardMethodStub, 9, {
  {"method", 1, 0, convertToTclobj},
  {"-default", 0, 1, convertToTclobj},
  {"-earlybinding", 0, 0, convertToString},
  {"-methodprefix", 0, 1, convertToTclobj},
  {"-objscope", 0, 0, convertToString},
  {"-onerror", 0, 1, convertToTclobj},
  {"-verbose", 0, 0, convertToString},
  {"target", 0, 0, convertToTclobj},
  {"args", 0, 0, convertToNothing}}
},
{"::xotcl::cmd::Object::instvar", XOTclOInstVarMethodStub, 1, {
  {"args", 0, 0, convertToNothing}}
},
{"::xotcl::cmd::Object::invar", XOTclOInvariantsMethodStub, 1, {
  {"invariantlist", 1, 0, convertToTclobj}}
},
{"::xotcl::cmd::Object::method", XOTclOMethodMethodStub, 7, {
  {"-inner-namespace", 0, 0, convertToString},
  {"-protected", 0, 0, convertToString},
  {"name", 1, 0, convertToTclobj},
  {"args", 1, 0, convertToTclobj},
  {"body", 1, 0, convertToTclobj},
  {"-precondition", 0, 1, convertToTclobj},
  {"-postcondition", 0, 1, convertToTclobj}}
},
{"::xotcl::cmd::Object::mixinguard", XOTclOMixinGuardMethodStub, 2, {
  {"mixin", 1, 0, convertToString},
  {"guard", 1, 0, convertToTclobj}}
},
{"::xotcl::cmd::Object::__next", XOTclONextMethodStub, 1, {
  {"args", 0, 0, convertToNothing}}
},
{"::xotcl::cmd::Object::noinit", XOTclONoinitMethodStub, 0, {
  }
},
{"::xotcl::cmd::Object::procsearch", XOTclOProcSearchMethodStub, 1, {
  {"name", 1, 0, convertToString}}
},
{"::xotcl::cmd::Object::requireNamespace", XOTclORequireNamespaceMethodStub, 0, {
  }
},
{"::xotcl::cmd::Object::residualargs", XOTclOResidualargsMethodStub, 1, {
  {"args", 0, 0, convertToNothing}}
},
{"::xotcl::cmd::Object::setter", XOTclOSetterMethodStub, 1, {
  {"name", 1, 0, convertToString}}
},
{"::xotcl::cmd::Object::uplevel", XOTclOUplevelMethodStub, 1, {
  {"args", 0, 0, convertToNothing}}
},
{"::xotcl::cmd::Object::upvar", XOTclOUpvarMethodStub, 1, {
  {"args", 0, 0, convertToNothing}}
},
{"::xotcl::cmd::Object::volatile", XOTclOVolatileMethodStub, 0, {
  }
},
{"::xotcl::cmd::Object::vwait", XOTclOVwaitMethodStub, 1, {
  {"varname", 1, 0, convertToString}}
},
{"::xotcl::alias", XOTclAliasCmdStub, 6, {
  {"object", 0, 0, convertToObject},
  {"methodName", 0, 0, convertToString},
  {"-objscope", 0, 0, convertToString},
  {"-per-object", 0, 0, convertToString},
  {"-protected", 0, 0, convertToString},
  {"cmdName", 1, 0, convertToTclobj}}
},
{"::xotcl::configure", XOTclConfigureCmdStub, 2, {
  {"configureoption", 1, 0, convertToConfigureoption},
  {"value", 0, 0, convertToTclobj}}
},
{"::xotcl::createobjectsystem", XOTclCreateObjectSystemCmdStub, 2, {
  {"rootClass", 1, 0, convertToTclobj},
  {"rootMetaClass", 1, 0, convertToTclobj}}
},
{"::xotcl::deprecated", XOTclDeprecatedCmdStub, 3, {
  {"what", 1, 0, convertToString},
  {"oldCmd", 1, 0, convertToString},
  {"newCmd", 0, 0, convertToString}}
},
{"::xotcl::dispatch", XOTclDispatchCmdStub, 4, {
  {"object", 1, 0, convertToObject},
  {"-objscope", 0, 0, convertToString},
  {"command", 1, 0, convertToTclobj},
  {"args", 0, 0, convertToNothing}}
},
{"::xotcl::dot", XOTclDotCmdStub, 1, {
  {"args", 0, 0, convertToNothing}}
},
{"::xotcl::finalize", XOTclFinalizeObjCmdStub, 0, {
  }
},
{"::xotcl::self", XOTclGetSelfObjCmdStub, 1, {
  {"selfoption", 0, 0, convertToSelfoption}}
},
{"::xotcl::instvar", XOTclInstvarCmdStub, 2, {
  {"-object", 0, 1, convertToObject},
  {"args", 0, 0, convertToNothing}}
},
{"::xotcl::interp", XOTclInterpObjCmdStub, 2, {
  {"name", 0, 0, convertToString},
  {"args", 0, 0, convertToNothing}}
},
{"::xotcl::is", XOTclIsCmdStub, 3, {
  {"object", 1, 0, convertToTclobj},
  {"objectkind", 0, 0, convertToObjectkind},
  {"value", 0, 0, convertToTclobj}}
},
{"::xotcl::methodproperty", XOTclMethodPropertyCmdStub, 5, {
  {"object", 1, 0, convertToObject},
  {"methodName", 1, 0, convertToString},
  {"-per-object", 0, 0, convertToString},
  {"methodproperty", 1, 0, convertToMethodproperty},
  {"value", 0, 0, convertToTclobj}}
},
{"::xotcl::my", XOTclMyCmdStub, 3, {
  {"-local", 0, 0, convertToString},
  {"method", 1, 0, convertToTclobj},
  {"args", 0, 0, convertToNothing}}
},
{"::xotcl::namespace_copycmds", XOTclNSCopyCmdsStub, 2, {
  {"fromNs", 1, 0, convertToTclobj},
  {"toNs", 1, 0, convertToTclobj}}
},
{"::xotcl::namespace_copyvars", XOTclNSCopyVarsStub, 2, {
  {"fromNs", 1, 0, convertToTclobj},
  {"toNs", 1, 0, convertToTclobj}}
},
{"::xotcl::__qualify", XOTclQualifyObjCmdStub, 1, {
  {"name", 1, 0, convertToTclobj}}
},
{"::xotcl::relation", XOTclRelationCmdStub, 4, {
  {"object", 0, 0, convertToObject},
  {"-per-object", 0, 0, convertToString},
  {"relationtype", 1, 0, convertToRelationtype},
  {"value", 0, 0, convertToTclobj}}
},
{"::xotcl::setinstvar", XOTclSetInstvarCmdStub, 3, {
  {"object", 1, 0, convertToObject},
  {"variable", 1, 0, convertToTclobj},
  {"value", 0, 0, convertToTclobj}}
}
};

