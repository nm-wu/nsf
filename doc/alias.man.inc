[comment {-*- tcl -*- manpage fragment for alias method, shared by nx::Object and nx::Class}]

[keywords "alias method"]
[keywords "value checker"]
[keywords "method handle"]

[call [arg [vset CMD]] [opt "[method public] | [method private] | [method protected]"] [method "[vset MODIFIER] alias"] [opt [option -debug]] [opt [option -deprecated]] [arg methodName] [opt "[option -returns] [arg valueChecker]"] [opt "[option -frame] [const object] | [const method]"] [arg cmdName]]

Define an [term "alias method"] for the given [vset SCOPE]. The
resulting method registers a pre-existing Tcl command [arg cmdName]
under the (alias) name [arg methodName] with the [vset SCOPE]. If [arg cmdName] refers
to another [method method], the corresponding argument
should be a valid [term "method handle"]. If a Tcl command (e.g., a
[cmd proc]), the argument should be a fully qualified Tcl command
name. If aliasing a subcommand (e.g., [cmd "array exists"]) of a Tcl namespace ensemble (e.g., [cmd array]), [arg cmdName] must hold the fully qualified subcommand name (and not the ensemble name of the subcommand).
[para] 
As for a regular [method "[vset SCOPE] method"], [option "-returns"]
allows for setting a [term "value checker"] on the values returned by
the aliased command [arg cmdName].
[para] 
When creating an [term "alias method"] for
a [emph C-implemented] Tcl command (i.e., command defined using the
Tcl/NX C-API), [option -frame] sets the scope
for variable references used in the aliased command. If the provided
value is [const object], then variable references will be resolved in the
context of the called object, i.e., the object upon which the [term "alias method"] is invoked, as if they were object variables. There is no need for using
the colon-prefix notation for identifying object variables. If the
value is [const method], then the aliased command will be executed as a regular method call. The command is aware of its called-object context; i.e., it can resolve [cmd ::nx::self]. In addition, the [term "alias method"] has access to the method-call context (e.g., [cmd nx::next]). If [option "-frame"] is omitted, and by default, the variable references will resolve in the context of the caller of the [term "alias method"].
[para]
To express deprecation of the [term "alias method"] [arg methodName], set the [option "-deprecated"] flag. Deprecated methods remain usable from client code, but their usage will be signaled to the developer and/or can be tracked using [cmd ::nsf::deprecated]. To register [arg methodName] with the debugger, set the [option "-debug"] flag.  Entering and exiting a method, which was flagged for debugging, is recorded by calling the redefinable callback procs [cmd ::nsf::debug::call] and [cmd ::nsf::debug::exit], respectively. By default, these callbacks forward to [cmd ::nsf::log], which can also be customized at the script level.

