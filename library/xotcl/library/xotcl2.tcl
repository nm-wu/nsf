############################################################
# xotcl2.tcl -
#
#      Implementation of the XOTcl 2 object systen, based
#      on the Next Scripting Framework
#
# Copyright (C) 2010-2011 Gustaf Neumann
# Copyright (C) 2010-2011 Stefan Sobernig
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation files
# (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#

package provide XOTcl 2.0b4
package require nx

#######################################################
# Classical ::xotcl*
#######################################################
namespace eval ::xotcl {
  #
  # Set XOTcl version variables
  #
  set ::xotcl::version 2.0
  set ::xotcl::patchlevel .0

  namespace eval ::nsf {}; # make package indexer (pkg_mkIndex) happy
  set ::nsf::bootstrap ::xotcl

  #
  # Perform the basic setup of XOTcl. First, let us allocate the
  # basic classes of XOTcl. This call creates the classes
  # ::xotcl::Object and ::xotcl::Class and defines these as root class
  # of the object system and as root meta class.
  #
  ::nsf::objectsystem::create ::xotcl::Object ::xotcl::Class {
    -class.alloc alloc
    -class.create create
    -class.dealloc dealloc
    -class.objectparameter objectparameter
    -class.recreate recreate
    -object.configure configure
    -object.cleanup cleanup
    -object.defaultmethod defaultmethod
    -object.destroy destroy
    -object.init init
    -object.move move
    -object.unknown unknown
  }

  #
  # create ::nx and ::nsf namespaces, otherwise mk_pkgindex will fail
  #
  namespace eval ::nx {}
  namespace eval ::nsf {}
  namespace eval ::nsf::method::create {}

  #
  # get frequently used primitiva into the ::xotcl namespace
  #
  namespace import ::nsf::configure ::nsf::my ::nsf::finalize ::nsf::interp
  namespace import ::nsf::method::alias ::nsf::is ::nsf::relation
  interp alias {} ::xotcl::next {} ::nsf::xotclnext

  proc ::xotcl::self {{arg ""}} {
      switch $arg {
	"" {uplevel ::nsf::self}
	next {
	  set handle [uplevel ::nsf::current nextmethod]
	  method_handle_to_xotcl $handle
	}
	default {uplevel ::nsf::current $arg}
      }
  }

  # @object ::xotcl::Object
  #
  # XOTcl programs are constructed out of objects. This class
  # describes common structural and behavioral features for all XOTcl
  # objects. It is the root object-class in the XOTcl 2 object system.

  # provide the standard command set for ::xotcl::Object
  foreach cmd [info command ::nsf::methods::object::*] {
    set cmdName [namespace tail $cmd]
    if {$cmdName in [list "setter" "requirenamespace"]} continue
    ::nsf::method::alias Object $cmdName $cmd
  }

  #
  # object methods
  #

  # @method ::xotcl::Object#autoname
  #
  # Provides a facility for auto-generating object identifiers. It is
  # constructed from a seeding string which is appended a numeric
  # index. This numeric index is incremented upon each call to
  # {{{autoname}}}.
  # {{{
  #       set obj [Object new]
  #       $obj autoname a; # yields "a1"
  #       $obj autoname -instance B; # yields "b1"
  #       $obj autoname a; # yields "a2"
  #       $obj autoname b; # yields "b1"
  #       $obj autoname -reset a; # ""
  #       $obj autoname -reset -instance B; # ""
  #       $obj autoname -instance a; # yields "a1", and NOT "a3"!
  #       $obj autoname -instance B; # yields "b1"
  #       $obj autoname b; # yields "b2"
  # }}}
  # The seeding string may also contain {{{[format]}}} expressions (see ...):
  # {{{
  #       $obj autoname a%06d; # gives you "a000001", ...
  # }}}
  #
  # @param -instance Have the generated name start with a lower letter (though the seed string has a major first letter)
  # @param -reset Reset the object-internal counter for a given seed string
  # @param name The seeding string which is used as a base for name generation
  # @return The generated name string

  # @method ::xotcl::Object#cleanup
  #
  # TODO: this is a method not used in the Next Scripting Langauge. This
  # method is just called via recreate, so everything necessary can be
  # performed there as well. However, it is available for backward
  # compatibility available in XOTcl 2.0
  #
  # Resets an object or class to its initial state, as after object
  # allocation (see {{@method ::xotcl::Class class alloc}}). This method
  # participates in recreating objects, i.e, it is called during the
  # recreation process by {{@method ::xotcl::Class class recreate}}.
  # Depending on the recreation scheme applied (see {{@command
  # ::nsf::configure}}, object variables are deleted, per-object
  # namespaces are cleared, and the object's relationships (e.g., mixin
  # relations) are reset.
  #
  # @properties interally-called

  # @method ::xotcl::Object#destroy
  #
  # @use ::xotcl::Object#destroy

  # @method ::xotcl::Object#exists
  #
  # A helper method for checking whether the variable {{{var}}} is
  # defined on the object and assigned a value. You may use a variable
  # name with or without prefix, both will resolve to the object scope:
  # {{{
  #       $obj eval {
  #          set :foo 1
  #          set bar 2
  #       }
  #
  #       $obj exists foo; # returns 1
  #       $obj exists :foo; # returns 1
  #       $obj exists bar; # returns 0
  #       $obj exists :bar; # returns 0
  # }}}
  #
  # @param var The name of the variable to verify
  # @return :boolean 1 if the variable exists, 0 otherwise

  # @method ::xotcl::Object#instvar
  #
  # @param args

  # @method ::xotcl::Object#noinit
  #
  # Calling upon this method during object construction allows you to
  # bypass the constructor method:
  # {{{
  #    Class create C
  #    C instproc init {} {puts stderr "A class-specific constructor shouts out ..."}
  #    C c1 -noinit
  # }}}
  # This bypassing feature comes handy when streaming an object into a
  # scripted form (e.g., by using the bundled Serializer). Upon
  # deserializing the object, using the {{{noinit}}} flag helps you to
  # preserve the serialized object state (rather then having the
  # object re-initialized).

  # @method ::xotcl::Object#requireNamespace
  #
  # This method allows you to request the creation of a namespace for
  # the given object, a per-object namespace. The namespace is then used
  # to store instance variables, methods and nested objects. Per-object
  # namespaces are needed for using and binding object variables to
  # non-object scopes in Tcl and Tk. For instance, you may use an
  # per-object namespace to have object variables accessible Tk widgets
  # and Tk callbacks. To verify whether a per-object namespace is
  # available for an object, see ...
  #
  # Beware that there is a difference between per-object namespaces and
  # Tcl namespaces which shadow an existing object (i.e., carry the same
  # name):
  # {{{
  #    Object create Foo
  #    Foo requireNamespace
  #    namespace exists Foo; # returns 1
  #    Foo info hasnamespace; # returns 1
  #
  #    Object create Bar
  #    namespace eval ::Bar {}
  #    namespace exists Bar; # returns 1
  #    Bar info hasnamespace; # returns 0
  # }}}

  # provide some Tcl-commands as methods for ::xotcl::Object
  foreach cmd {array append eval incr lappend set subst unset trace} {
    ::nsf::method::alias Object $cmd -frame object ::$cmd
  }

  # @method ::xotcl::Object#vwait
  #
  # A method variant of the Tcl {{{vwait}}} command. You can use it to
  # have the {{{interp}}} enter an event loop until the specified
  # variable {{{varname}}} is set on the object.
  #
  # @param varName The name of the signaling object variable.

  ::nsf::method::create Object vwait {varName} {
    if {[regexp {:[^:]*} $varName]} {
      error "invalid varName '$varName'; only plain or fully qualified variable names allowed"
    }
    if {[string match ::* $varName]} {
      ::vwait $varName
    } else {
      ::vwait :$varName
    }
  }

  # provide the standard command set for ::xotcl::Class
  foreach cmd [info command ::nsf::methods::class::*] {
    set cmdName [namespace tail $cmd]
    if {$cmdName in [list "setter"]} continue
    ::nsf::method::alias Class $cmdName $cmd
  }

  # protect some methods against redefinition
  ::nsf::method::property Object destroy redefine-protected true
  ::nsf::method::property Class  alloc   redefine-protected true
  ::nsf::method::property Class  dealloc redefine-protected true
  ::nsf::method::property Class  create  redefine-protected true

  #
  # define parametercmd and instparametercmd in terms of ::nsf::setter
  # define filterguard and instfilterguard in terms of filterguard
  # define mixinguard and instmixinguard in terms of mixinguard
  #
  ::nsf::method::forward Object parametercmd ::nsf::method::setter %self -per-object
  ::nsf::method::forward Class instparametercmd ::nsf::method::setter %self

  ::nsf::method::alias Object filterguard      ::nsf::methods::object::filterguard
  ::nsf::method::alias Class  instfilterguard  ::nsf::methods::class::filterguard

  ::nsf::method::alias Object mixinguard       ::nsf::methods::object::mixinguard
  ::nsf::method::alias Class  instmixinguard   ::nsf::methods::class::mixinguard

  ::nsf::method::alias Object requireNamespace ::nsf::methods::object::requirenamespace

  # define instproc and proc
  ::nsf::method::create Class instproc {
    name arguments:parameter,0..* body precondition:optional postcondition:optional
  } {
    set conditions [list]
    if {[info exists precondition]}  {lappend conditions -precondition  $precondition}
    if {[info exists postcondition]} {lappend conditions -postcondition $postcondition}
    ::nsf::method::create [self] $name $arguments $body {*}$conditions
  }

  ::nsf::method::create Object proc {
    name arguments body precondition:optional postcondition:optional
  } {
    set conditions [list]
    if {[info exists precondition]}  {lappend conditions -precondition  $precondition}
    if {[info exists postcondition]} {lappend conditions -postcondition $postcondition}
    ::nsf::method::create [self] -per-object $name $arguments $body {*}$conditions
  }

  # define a minimal implementation of "method"
  Object instproc method {name arguments:parameter,0..* body} {
    :proc $name $arguments $body
  }
  Class instproc method {-per-object:switch name arguments:parameter,0..* body} {
    if {${per-object}} {
      :proc $name $arguments $body
    } else {
      :instproc $name $arguments $body
    }
  }

  # define forward methods
  #
  # We could nearly define forward via forwarder
  #
  #    ::nsf::method::forward Object forward ::nsf::method::forward %self -per-object
  #    ::nsf::method::forward Class instforward ::nsf::method::forward %self
  #
  # but since we changed the name of -objscope in nsf to -objframe, we
  # have to provide the definition the hard way via methods.

  Object instproc forward {
    method
    -default -earlybinding:switch -methodprefix -objscope:switch -onerror -verbose:switch
    target:optional args
  } {
    set arglist [list]
    if {[info exists default]} {lappend arglist -default $default}
    if {$earlybinding} {lappend arglist "-earlybinding"}
    if {[info exists methodprefix]} {lappend arglist -methodprefix $methodprefix}
    if {$objscope} {lappend arglist "-objframe"}
    if {[info exists onerror]} {lappend arglist -onerror $onerror}
    if {$verbose} {lappend arglist -verbose}
    if {[info exists target]} {lappend arglist $target}
    if {[llength $args] > 0} {lappend arglist {*}$args}
    set r [::nsf::method::forward [self] -per-object $method {*}$arglist]
    return $r
  }

  Class instproc instforward {
    method
    -default -earlybinding:switch -methodprefix -objscope:switch -onerror -verbose:switch
    target:optional args
  } {
    set arglist [list]
    if {[info exists default]} {lappend arglist -default $default}
    if {$earlybinding} {lappend arglist "-earlybinding"}
    if {[info exists methodprefix]} {lappend arglist -methodprefix $methodprefix}
    if {$objscope} {lappend arglist "-objframe"}
    if {[info exists onerror]} {lappend arglist -onerror $onerror}
    if {$verbose} {lappend arglist -verbose}
    if {[info exists target]} {lappend arglist $target}
    if {[llength $args] > 0} {lappend arglist {*}$args}
    set r [::nsf::method::forward [self] $method {*}$arglist]
    return $r
  }


  Class instproc unknown {args} {
    #puts stderr "use '[self] create $args', not '[self] $args'"
    uplevel [list [self] create {*}$args]
  }

  Object instproc unknown {m args} {
    if {![self isnext]} {
      error "[self]: unable to dispatch method '$m'"
    }
  }

  # "init" must exist on Object. per default it is empty.
  Object instproc init args {
    if {![::nsf::current isnextcall] && [llength $args] > 0 && [::nsf::configure debug] > 0} {
      ::nsf::log Warning "Arguments '$args' to constructor of object [self] are most likely not processed"
    }
  }

  Object instproc self {} {::xotcl::self}

  #
  # Method objectparameter, backwards upward compatible. We use
  # here the definition of parametersfromslots from nx.tcl
  #
  ::xotcl::Class instproc objectparameter {} {
    set parameterdefinitions [list]
    foreach slot [nsf::directdispatch [self] ::nsf::methods::class::info::slotobjects -closure -type ::nx::Slot] {
      lappend parameterdefinitions [$slot getParameterSpec]
    }
    lappend parameterdefinitions args:alias,method=residualargs,args
    return $parameterdefinitions
  }

  ######################################################################
  # Define default property protection before calling :property
  ######################################################################
  ::nsf::method::create ::xotcl::Object __default_property_call_protection args {return false}
  ::nsf::method::property ::xotcl::Object __default_property_call_protection call-protected true


  #
  # Use parameter definition from nx
  # (same with classInfo parameter, see below)
  #::nsf::method::alias ::xotcl::Class parameter ::nsf::classes::nx::Class::attributes

  ::xotcl::Class instproc parameter {arglist} {
    set slotContainer [::nx::slotObj [::nsf::self]]
    foreach arg $arglist {
      #puts stderr "[self] ::nsf::classes::nx::Class::property $arg"
      [self] ::nsf::classes::nx::Class::property $arg
    }
    ::nsf::var::set $slotContainer __parameter $arglist
  }

  # We provide a default value for superclass (when no superclass is
  # specified explicitly) and metaclass, in case they should differ
  # from the root classes of the object system.

  ::xotcl::Class parameter {
    {__default_superclass ::xotcl::Object}
    {__default_metaclass ::xotcl::Class}
  }

  ############################################
  # Register system slots
  ############################################

  # We need fully qualified "::xotcl" prefixes, since prefix
  # completion would skip the object system root namespace

  set cSlotContainer [::nx::slotObj ::xotcl::Class]
  set oSlotContainer [::nx::slotObj ::xotcl::Object]
  ::nx::RelationSlot create ${cSlotContainer}::superclass
  ::nsf::method::alias      ${cSlotContainer}::superclass assign ::nsf::relation
  ::nx::RelationSlot create ${oSlotContainer}::class -elementtype class -multiplicity 1..1
  ::nsf::method::alias      ${oSlotContainer}::class assign ::nsf::relation
  ::nx::RelationSlot create ${oSlotContainer}::mixin  -forwardername object-mixin \
      -elementtype mixinreg -multiplicity 0..n
  ::nx::RelationSlot create ${oSlotContainer}::filter -forwardername object-filter \
      -elementtype filterreg -multiplicity 0..n
  ::nx::RelationSlot create ${cSlotContainer}::instmixin  -forwardername class-mixin \
      -elementtype mixinreg -multiplicity 0..n
  ::nx::RelationSlot create ${cSlotContainer}::instfilter -forwardername class-filter \
	-elementtype filterreg -multiplicity 0..n

  ########################
  # Info definition
  ########################
  Object create ::xotcl::objectInfo
  Object create ::xotcl::classInfo
  ::nsf::object::property ::xotcl::objectInfo keepcallerself true
  ::nsf::object::property ::xotcl::classInfo  keepcallerself true
  ::nsf::object::property ::xotcl::objectInfo perobjectdispatch true
  ::nsf::object::property ::xotcl::classInfo  perobjectdispatch true


  # note, we are using ::xotcl::infoError, defined below
  #Object instforward info -onerror ::nsf::infoError ::xotcl::objectInfo %1 {%@2 %self}
  #Class  instforward info -onerror ::nsf::infoError ::xotcl::classInfo %1 {%@2 %self}
  #
  # error handler for info
  #
  #proc ::nsf::infoerror msg {
  #  #puts stderr "INFO ERROR: <$msg>\n$::errorInfo"
  #  regsub -all " <object>" $msg "" msg
  #  regsub -all " <class>" $msg "" msg
  #  regsub {\"} $msg "\"info " msg
  #  error $msg ""
  #}

  ::nsf::method::alias Object info ::xotcl::objectInfo
  ::nsf::method::alias Class info ::xotcl::classInfo

  #
  # Backward compatibility info subcommands;
  #
  # TODO: should go finally into a library.
  #
  # Obsolete methods
  #
  #   already emulated:
  #
  #    => info -per-object method parameter .... replaces
  #     info instargs
  #     info instnonposargs
  #     info instdefault
  #
  #    => info method .... replaces
  #     info body
  #     info instbody
  #
  #    => info methods .... replaces
  #     info commands
  #     info instcommands
  #     info procs
  #     info instprocs
  #     info parametercmd
  #     info instparametercmd
  #
  #    => info is (resp. ::xotcl::is) replaces
  #     info isobject
  #     info isclass
  #     info ismetaclass
  #     info ismixin
  #     info istype
  #
  #    => info method .... replaces
  #     proc
  #     instproc
  #     info args
  #     info nonposargs
  #     info default
  #
  # TODO mark all obsolete calls at least as deprecated in library
  #

  proc ::xotcl::info_args {scope o method} {
    set result [list]
    foreach \
        argName [$o ::nsf::methods::${scope}::info::method args $method] \
        flag    [$o ::nsf::methods::${scope}::info::method parameter $method] {
          if {[string match -* $flag]} continue
          lappend result $argName
        }
    #puts stderr "+++ get ${inst}args for $o $method => $result"
    return $result
  }

  proc ::xotcl::info_nonposargs {scope o method} {
    set result [list]
    foreach flag [$o ::nsf::methods::${scope}::info::method parameter $method] {
      if {![string match -* $flag]} continue
      lappend result $flag
    }
    #puts stderr "+++ get ${inst}nonposargs for $o $method => $result"
    return $result
  }
  proc ::xotcl::info_default {scope o method arg varName} {
    foreach \
        argName [$o ::nsf::methods::${scope}::info::method args $method] \
        flag    [$o ::nsf::methods::${scope}::info::method parameter $method] {
          if {$argName eq $arg} {
	    # upvar 2 $varName default
	    # use "my" here to avoid surprises with aliases or interceptors
	    $o upvar $varName default
	    #puts "--- info_default var '$varName' level=[info level]"
            if {[llength $flag] == 2} {
              set default [lindex $flag 1]
              #puts stderr "--- get $scope default for $o $method $arg => $default"
              return 1
            }
            #puts stderr "--- get $scope default for $o $method $arg fails"
            set default ""
            return 0
          }
        }
    error "procedure \"$method\" doesn't have an argument \"$varName\""
  }

  proc ::xotcl::info_forward_options {list} {
    set result [list]
    set i 0
    foreach w $list {
      switch -glob -- $w {
	-objframe {lappend result -objscope}
	-* {lappend result $w}
	default {
	  lappend result {*}[lrange $list $i end]
	  break
	}
      }
      incr i
    }
    return $result
  }

  # define temporary method "alias"
  Object instproc alias {name cmd} {::nsf::method::alias [self] $name $cmd}

  objectInfo eval {
    :proc args {method}       {::xotcl::info_args object [self] $method}
    :proc body {methodName}   {my ::nsf::methods::object::info::method body $methodName}
    :proc check {}            {::xotcl::checkoption_internal_to_xotcl1 [::nsf::method::assertion [self] check]}
    :alias class              ::nsf::methods::object::info::class
    :alias children           ::nsf::methods::object::info::children
    :proc commands {pattern:optional} {
      set cmd [list ::nsf::methods::object::info::methods -methodtype all]
      if {[info exists pattern]} {lappend cmd $pattern}
      return [my {*}$cmd]
    }
    :proc default {method arg varName} {
      # pass varName to be able produce the right error message
      set r [::xotcl::info_default object [self] $method $arg $varName]
      #puts "--- var '$varName' level=[info level]"
      return $r
    }
    :proc filter {-order:switch -guards:switch pattern:optional} {
      set guardsFlag [expr {$guards ? "-guards" : ""}]
      set patternArg [expr {[info exists pattern] ? [list $pattern] : ""}]
      if {$order && !$guards} {
        set def [::nsf::directdispatch [::nsf::current object] \
		     ::nsf::methods::object::info::filtermethods -order \
		     {*}$guardsFlag \
		     {*}$patternArg]
        set def [method_handles_to_xotcl $def]
      } else {
        set def [::nsf::directdispatch [::nsf::current object] \
		     ::nsf::methods::object::info::filtermethods \
		     {*}$guardsFlag \
		     {*}$patternArg]
      }
      #puts stderr "  => $def"
      return $def
    }

    :alias filterguard        ::nsf::methods::object::info::filterguard
    :proc forward {-definition:switch name:optional} {
      if {$definition} {
	if {![info exists name]} {error "option -definition requires name of forwarding method to be specified" }
	set def [my ::nsf::methods::object::info::forward -definition $name]
	return [::xotcl::info_forward_options $def]
      } else {
	return [my ::nsf::methods::object::info::forward {*}[self args]]
      }
    }
    :alias hasnamespace       ::nsf::methods::object::info::hasnamespace
    :proc invar {}            {::nsf::method::assertion [self] object-invar}

    :proc methods {
      -nocmds:switch -noprocs:switch -nomixins:switch -incontext:switch pattern:optional
    } {
      set methodtype all
      if {$nocmds} {set methodtype scripted}
      if {$noprocs} {if {$nocmds} {return ""}; set methodtype builtin}
      set cmd [list ::nsf::methods::object::info::lookupmethods -methodtype $methodtype]
      if {$nomixins} {lappend cmd -nomixins}
      if {$incontext} {lappend cmd -incontext}
      if {[info exists pattern]} {lappend cmd $pattern}
      my {*}$cmd
    }

    :proc mixin {-order:switch -guards:switch pattern:optional} {
      set cmd ::nsf::methods::object::info::mixinclasses
      if {$order} {lappend cmd "-heritage"}
      if {$guards} {lappend cmd "-guards"}
      if {[info exists pattern]} {lappend cmd $pattern}
      my {*}$cmd
    }
    :alias mixinguard         ::nsf::methods::object::info::mixinguard
    :proc nonposargs {method} {::xotcl::info_nonposargs object [self] $method}
    :proc parametercmd {name} {::nsf::classes::nx::Object::setter [self] $name}
    :alias parent             ::nsf::methods::object::info::parent
    :proc post {methodName}   {my ::nsf::methods::object::info::method post $methodName}
    :proc pre  {methodName}   {my ::nsf::methods::object::info::method pre  $methodName}
    :proc procs {pattern:optional} {
      set cmd [list ::nsf::methods::object::info::methods -methodtype scripted]
      if {[info exists pattern]} {lappend cmd $pattern}
      return [my {*}$cmd]
    }
    :alias slots              ::nsf::methods::object::info::slotobjects
    :alias precedence         ::nsf::methods::object::info::precedence
    :alias vars               ::nsf::methods::object::info::vars
  }

  #
  # copy all methods from Object.info to Class.info
  #
  foreach m [objectInfo ::nsf::methods::object::info::methods] {
    ::nsf::method::alias classInfo $m [objectInfo ::nsf::methods::object::info::method registrationhandle $m]
  }

  classInfo eval {
    :alias classchildren      ::nsf::methods::object::info::children
    :alias classparent        ::nsf::methods::object::info::parent
    :proc default {method arg varName} {
      set r [::xotcl::info_default object [self] $method $arg $varName]
      #puts "--- var '$varName' level=[info level]"
      return $r
    }
    :proc heritage {pattern:optional} {
      set cmd [list ::nsf::methods::class::info::superclass -closure]
      if {[info exists pattern]} {lappend cmd $pattern}
      return [my {*}$cmd]	
    }
    :alias instances          ::nsf::methods::class::info::instances

    :proc instargs {method}   {::xotcl::info_args class [self] $method}
    :proc instbody {methodName} {my ::nsf::methods::class::info::method body $methodName}
    :proc instcommands {pattern:optional}  {
      set cmd [list ::nsf::methods::class::info::methods]
      if {[info exists pattern]} {lappend cmd $pattern}
      return [my {*}$cmd]
    }
    :proc instdefault {method arg varName} {
      set r [::xotcl::info_default class [self] $method $arg $varName]
      return $r
    }
    :alias instfilter         ::nsf::methods::class::info::filtermethods
    :alias instfilterguard    ::nsf::methods::class::info::filterguard
    #:alias instforward        ::nsf::methods::class::info::forward
    :proc instforward {-definition:switch name:optional} {
      if {$definition} {
	if {![info exists name]} {error "option -definition requires name of forwarding method to be specified" }
	set def [my ::nsf::methods::class::info::forward -definition $name]
	return [::xotcl::info_forward_options $def]
      } else {
	return [my ::nsf::methods::class::info::forward {*}[self args]]
      }
    }
    :proc instinvar {} {::nsf::method::assertion [self] class-invar}
    :proc instmixin {-order:switch -guards:switch pattern:optional} {
      set cmd ::nsf::methods::class::info::mixinclasses
      if {$order} {lappend cmd "-heritage"}
      if {$guards} {lappend cmd "-guards"}
      if {[info exists pattern]} {lappend cmd $pattern}
      my {*}$cmd
    }
    :alias instmixinguard     ::nsf::methods::class::info::mixinguard
    :proc instmixinof {-closure:switch pattern:optional} {
      set cmd [list ::nsf::methods::class::info::mixinof -scope class]
      if {$closure} {lappend cmd -closure}
      if {[info exists pattern]} {lappend cmd $pattern}
      return [my {*}$cmd]
    }
    :proc instparametercmd {pattern:optional} {
      set cmd [list ::nsf::methods::class::info::methods -methodtype setter]
      if {[info exists pattern]} {lappend cmd $pattern}
      return [my {*}$cmd]
    }
    :proc instnonposargs {method} {::xotcl::info_nonposargs class [self] $method}
    :proc instpost {methodName}   {my ::nsf::methods::class::info::method postcondition $methodName}
    :proc instpre  {methodName}   {my ::nsf::methods::class::info::method precondition  $methodName}

    :proc instprocs {pattern:optional} {
      set cmd [list ::nsf::methods::class::info::methods -methodtype scripted]
      if {[info exists pattern]} {lappend cmd $pattern}
      return [my {*}$cmd]
    }
    :proc mixinof {-closure:switch pattern:optional} {
      set cmd [list ::nsf::methods::class::info::mixinof -scope object]
      if {$closure} {lappend cmd -closure}
      if {[info exists pattern]} {lappend cmd $pattern}
      return [my {*}$cmd]
    }
    #:alias parameter          ::nx::Class::slot::__info::attributes
    :proc parameter {} {
      set slotContainer [::nx::slotObj [::nsf::self]]
      if {[::nsf::var::exists $slotContainer __parameter]} {
	return [::nsf::var::set $slotContainer __parameter]
      }
      return ""
    }

    :alias slots              ::nsf::methods::class::info::slotobjects
    :alias subclass           ::nsf::methods::class::info::subclass
    :alias superclass         ::nsf::methods::class::info::superclass
  }

  # define "info info"
  objectInfo method info {} {::nx::internal::infoOptions ::xotcl::objectInfo}
  classInfo  method info {} {::nx::internal::infoOptions ::xotcl::classInfo}

  # define "info unknown"
  objectInfo proc unknown {method args} {
    error "[::xotcl::self] unknown info option \"$method\"; [:info info]"
  }
  classInfo proc unknown {method args} {
    error "[::xotcl::self] unknown info option \"$method\"; [:info info]"
  }

  #
  # end if info
  #

  # remove temporary method "alias"
  Object instproc alias {} {}

 # emulation of object::exists, isclass ...
  Object instproc isobject    {{object:substdefault "[self]"}} {::nsf::object::exists $object}
  Object instproc isclass     {{class:substdefault  "[self]"}} {::nsf::is class $class}
  Object instproc ismetaclass {{class:substdefault  "[self]"}} {::nsf::is metaclass $class}
  Object instproc ismixin     {class}  {
    return [expr {[::nsf::is class $class] &&
		  [my ::nsf::methods::object::info::hasmixin $class]}]
  }
  Object instproc istype      {class}  {
    return [expr {[::nsf::is class $class] &&
		  [::nsf::directdispatch [self] ::nsf::methods::object::info::hastype $class]}]
  }

  # definition of "xotcl::Object contains", based on nx
  ::nsf::method::alias Object contains ::nsf::classes::nx::Object::contains

  # definition of "xotcl::Class slots", based on contains
  ::xotcl::Class instproc slots {cmd} {
    set slotContainer [::nx::slotObj [self]]
    uplevel [list [self] contains -object $slotContainer $cmd]
  }

  # assertion handling
  proc checkoption_xotcl1_to_internal checkoptions {
    set options [list]
    foreach option $checkoptions {
      if {$option eq "invar"} {
        lappend options "object-invar"
      } elseif {$option eq "instinvar"} {
        lappend options "class-invar"
      } else {
        lappend options $option
      }
    }
    return $options
  }
  proc checkoption_internal_to_xotcl1 checkoptions {
    set options [list]
    foreach option $checkoptions {
      if {$option eq "object-invar"} {
        lappend options "invar"
      } elseif {$option eq "class-invar"} {
        lappend options "instinvar"
      } else {
        lappend options $option
      }
    }
    return $options
  }
  proc method_handles_to_xotcl definitions {
    set defs [list]
    foreach def $definitions {lappend defs [method_handle_to_xotcl $def]}
    return $defs
  }
  proc method_handle_to_xotcl methodHandle {
    set definition [::nx::Object info method definition $methodHandle]
    #puts "method_handle_to_xotcl raw definition '$methodHandle' // $definition"
    if {$definition ne ""} {
      set obj [lindex $definition 0]
      set modifier [lindex $definition 2]
	if {$modifier eq "class-object"} {
        set prefix ""
        set kind [lindex $definition 3]
        set name [lindex $definition 4]
      } else {
	set prefix [expr {[::nsf::is class $obj] ? "inst" : ""}]
        set kind $modifier
        set name [lindex $definition 3]
      }
      if {$kind eq "method"} {
        set kind proc
      } elseif {$kind eq "setter"} {
        set kind parametercmd
      } elseif {$kind eq "alias"} {
	set kind "cmd"
	set name [lindex $definition 3]
      }
      set definition [list [lindex $definition 0] ${prefix}$kind $name]
    }
    #puts "method_handle_to_xotcl gets handle '$methodHandle' // $definition"
    return $definition
  }


  Object instproc check {checkoptions} {
    ::nsf::method::assertion [self] check [::xotcl::checkoption_xotcl1_to_internal $checkoptions]
  }
  Object instforward invar     ::nsf::method::assertion %self object-invar
  Class  instforward instinvar ::nsf::method::assertion %self class-invar

  Object instproc abstract {methtype methname arglist} {
    if {$methtype ne "proc" && $methtype ne "instproc" && $methtype ne "method"} {
      error "invalid method type '$methtype', \
	must be either 'proc', 'instproc' or 'method'."
    }
    :$methtype $methname $arglist "
      if {!\[::xotcl::self isnextcall\]} {
        error \"Abstract method $methname $arglist called\"
      } else {::xotcl::next}
    "
  }

  # support for XOTcl specific convenience routines
  Object instproc hasclass cl {
    if {![::nsf::is class $cl]} {return 0}
    if {[::nsf::directdispatch [self] ::nsf::methods::object::info::hasmixin $cl]} {return 1}
    ::nsf::directdispatch [self] ::nsf::methods::object::info::hastype $cl
  }
  Object instproc filtersearch {filter} {
    set handle [::nsf::directdispatch [::nsf::current object] \
		    ::nsf::methods::object::info::lookupfilter $filter]
    return [method_handle_to_xotcl $handle]
  }
  Object instproc procsearch {name} {
    set handle [::nsf::directdispatch [::nsf::current object] \
		    ::nsf::methods::object::info::lookupmethod $name]
    return [method_handle_to_xotcl $handle]
  }
  Class instproc allinstances {} {
    # TODO: mark it deprecated
    return [:info instances -closure]
  }

  # keep old object interface for XOTcl
  Object proc unsetExitHandler {} {::nsf::exithandler unset}
  Object proc setExitHandler   {newbody} {::nsf::exithandler set $newbody}
  Object proc getExitHandler   {} {::nsf::exithandler get}

  # reuse some definitions from next scripting
  ::nsf::method::alias ::xotcl::Object copy ::nsf::classes::nx::Object::copy
  ::nsf::method::alias ::xotcl::Object move ::nsf::classes::nx::Object::move
  #::nsf::method::alias ::xotcl::Object defaultmethod ::nsf::classes::nx::Object::defaultmethod

  #::nsf::method::alias ::xotcl::Class -per-object __unknown ::nx::Class::__unknown
  ::nsf::method::create ::xotcl::Class -per-object __unknown {name} {}
  ::nsf::object::unknown::add xotcl {::xotcl::Class __unknown}

  proc myproc {args} {linsert $args 0 [::xotcl::self]}
  proc myvar  {var}  {:requireNamespace; return [::xotcl::self]::$var}

  #
  # create ::xotcl::MetaSlot for better compatibility with XOTcl 1
  #
  ::nx::Class create ::xotcl::MetaSlot -superclass ::nx::MetaSlot {
    :property parameter
    :method init {} {
      if {[info exists :parameter]} {my ::nsf::classes::xotcl::Class::parameter ${:parameter}}
      next
    }
    # provide minimal compatibility
    :public forward instproc %self public method
    :public forward proc %self public class method
    #
    # As NX/XOTcl hybrids, all slot kinds would not inherit the
    # unknown behaviour of ::xotcl::Class. Therefore, we provide it
    # explicitly to slots for backward compatibility ...
    #
    :public alias unknown ::nsf::classes::xotcl::Class::unknown
  }

  #
  # Create ::xotcl::Attribute for compatibility
  #
  ::xotcl::MetaSlot create ::xotcl::Attribute -superclass ::nx::VariableSlot {
    :property multivalued {
      :public method assign {object property value} {
	set mClass [expr {$value?"0..n":"1..1"}]
	$object incremental $value
	$object multiplicity $mClass
      }
      :public method get {object property} {
	return [$object eval [list :isMultivalued]]
      }
    }
  }

  #
  # Provide a backward compatible version of ::xotcl::alias
  #
  ::nsf::proc ::xotcl::alias {
    obj:object
    methodName
    -per-object:switch
    -objscope:switch
    target
  } {
    ::nsf::method::alias \
	$obj \
	{*}[expr {${per-object} ? "-per-object" : ""}] \
	$methodName \
	{*}[expr {${objscope} ? "-frame object" : ""}] \
	$target
  }

  Object create ::xotcl::config
  config proc load {obj file} {
    source $file
    foreach i [array names ::auto_index [list $obj *proc *]] {
      set type [lindex $i 1]
      set meth [lindex $i 2]
      if {[$obj info ${type}s $meth] == {}} {
        $obj $type $meth auto $::auto_index($i)
      }
  }
  }

  config proc mkindex {meta dir args} {
    set sp {[ 	]+}
    set st {^[ 	]*}
    set wd {([^ 	;]+)}
    foreach creator $meta {
      ::lappend cp $st$creator${sp}create$sp$wd
      ::lappend ap $st$creator$sp$wd
    }
    foreach methodkind {proc instproc} {
      ::lappend mp $st$wd${sp}($methodkind)$sp$wd
    }
    foreach cl [concat ::xotcl::Class [::xotcl::Class info heritage]] {
      ::lappend meths {*}[$cl info instcommands]
    }
    set old [pwd]
    cd $dir
    ::append idx "# Tcl autoload index file, version 2.0\n"
    ::append idx "# xotcl additions generated with "
    ::append idx "\"::xotcl::config::mkindex [list $meta] [list $dir] $args\"\n"
    set oc 0
    set mc 0
    foreach file [glob -nocomplain -- {*}$args] {
      if {[catch {set f [open $file]} msg]} then {
        catch {close $f}
        cd $old
        error $msg
      }
      while {[gets $f line] >= 0} {
        foreach c $cp {
          if {[regexp $c $line x obj]==1 &&
              [string index $obj 0]!={$}} then {
            ::incr oc
            ::append idx "set auto_index($obj) "
            ::append idx "\"::xotcl::config::load $obj \$dir/$file\"\n"
          }
        }
        foreach a $ap {
          if {[regexp $a $line x obj]==1 &&
              [string index $obj 0]!={$} &&
              [lsearch -exact $meths $obj]==-1} {
            ::incr oc
            ::append idx "set auto_index($obj) "
            ::append idx "\"::xotcl::config::load $obj \$dir/$file\"\n"
          }
        }
        foreach m $mp {
          if {[regexp $m $line x obj ty pr]==1 &&
              [string index $obj 0]!={$} &&
              [string index $pr 0]!={$}} then {
            ::incr mc
            ::append idx "set \{auto_index($obj "
            ::append idx "$ty $pr)\} \"source \$dir/$file\"\n"
          }
        }
      }
      close $f
    }
    set t [open tclIndex a+]
    puts $t $idx nonewline
    close $t
    cd $old
    return "$oc objects, $mc methods"
  }

  #
  # if cutTheArg not 0, it cut from upvar argsList
  #
  Object instproc extractConfigureArg {al name {cutTheArg 0}} {
    set value ""
    upvar $al argList
    set largs [llength $argList]
    for {set i 0} {$i < $largs} {incr i} {
      if {[lindex $argList $i] == $name && $i + 1 < $largs} {
        set startIndex $i
        set endIndex [expr {$i + 1}]
        while {$endIndex < $largs &&
               [string first - [lindex $argList $endIndex]] != 0} {
          lappend value [lindex $argList $endIndex]
          incr endIndex
        }
      }
    }
    if {[info exists startIndex] && $cutTheArg != 0} {
      set argList [lreplace $argList $startIndex [expr {$endIndex - 1}]]
    }
    return $value
  }

  Object create ::xotcl::rcs
  rcs proc date string {
    lreplace [lreplace $string 0 0] end end
  }
  rcs proc version string {
    lindex $string 2
  }

  #
  # package support
  #
  # puts this for the time being into XOTcl
  #
  ::xotcl::Class instproc uses list {
    foreach package $list {
      ::xotcl::package import -into [::xotcl::self] $package
      puts stderr "*** using ${package}::* in [::xotcl::self]"
    }
  }

  ::nx::Class create ::xotcl::package -superclass ::nx::Class {

    :property provide
    :property {version 1.0}
    :property {autoexport {}}
    :property {export {}}

    :public class method create {name args} {
      set nq [namespace qualifiers $name]
      if {$nq ne "" && ![namespace exists $nq]} {Object create $nq}
      next
    }

    :public class method extend {name args} {
      :require $name
	$name configure {*}$args
    }

    :public class method contains script {
      if {[info exists :provide]} {
        package provide [set :provide] [set :version]
      } else {
        package provide [::xotcl::self] [set :version]
      }
      namespace eval [::xotcl::self] {namespace import ::xotcl::*}
      #namespace eval [::xotcl::self] $script
      #::nsf::directdispatch [::xotcl::self] -frame method ::apply [list {} $script [::xotcl::self]]
      ::apply [list {} $script [::xotcl::self]]

      foreach e [set :export] {
        set nq [namespace qualifiers $e]
        if {$nq ne ""} {
          namespace eval [::xotcl::self]::$nq [list namespace export [namespace tail $e]]
        } else {
          namespace eval [::xotcl::self] [list namespace export $e]
        }
      }
      foreach e [set :autoexport] {
        namespace eval :: [list namespace import [::xotcl::self]::$e]
      }
    }

    :public class method unknown args {
      #puts stderr "unknown: package $args"
      [set :packagecmd] {*}$args
    }

    :public class method verbose value {
      set :verbose $value
    }

    :public class method present args {
      if {$::tcl_version<8.3} {
        switch -exact -- [lindex $args 0] {
          -exact  {set pkg [lindex $args 1]}
          default {set pkg [lindex $args 0]}
        }
        if {[info exists :loaded($pkg)]} {
          return ${:loaded}($pkg)
        } else {
          error "not found"
        }
      } else {
	[set :packagecmd] present {*}$args
      }
    }

    :public class method import {{-into ::} pkg} {
      :require $pkg
      namespace eval $into [subst -nocommands {
        #puts stderr "*** package import ${pkg}::* into [namespace current]"
        namespace import ${pkg}::*
      }]
      # import subclasses if any
      foreach e [$pkg export] {
        set nq [namespace qualifiers $e]
        if {$nq ne ""} {
          namespace eval $into$nq [list namespace import ${pkg}::$e]
        }
      }
    }

    :public class method require args {
      #puts "XOTCL package require $args, current=[namespace current]"
      set prevComponent ${:component}
      if {[catch {set v [package present {*}$args]} msg]} {
        #puts stderr "we have to load $msg"
        switch -exact -- [lindex $args 0] {
          -exact  {set pkg [lindex $args 1]}
          default {set pkg [lindex $args 0]}
        }
        set :component $pkg
        lappend :uses($prevComponent) ${:component}
        set v [uplevel \#1 [set :packagecmd] require $args]
        if {$v ne "" && ${:verbose}} {
        set path [lindex [::package ifneeded $pkg $v] 1]
          puts "... $pkg $v loaded from '$path'"
          set :loaded($pkg) $v   ;# loaded stuff needed for Tcl 8.0
        }
      }
      set :component $prevComponent
      return $v
    }

    set :component .
    set :verbose 0
    set :packagecmd ::package
  }

  unset -nocomplain cmd
  unset ::nsf::bootstrap

  # Documentation stub object -> just ignore per default.
  # if xoDoc is loaded, documentation will be activated
  ::xotcl::Object create ::xotcl::@
  ::xotcl::@ proc unknown args {}

  set ::xotcl::confdir ~/.xotcl
  set ::xotcl::logdir $::xotcl::confdir/log
  namespace import ::nsf::tmpdir

  # finally, export contents defined for XOTcl
  namespace export Object Class Attribute myproc myvar my self next @

  #
  # Provide parametersyntax for methods, which do not have a spec
  #
  # Tcl commands
  set ::nsf::parametersyntax(::append) "varName ?value value value ...?"
  set ::nsf::parametersyntax(::array) "option arrayName ?arg arg ...?"
  set ::nsf::parametersyntax(::eval) "arg ?arg ...?"
  set ::nsf::parametersyntax(::incr) "varName ?increment?"
  set ::nsf::parametersyntax(::lappend) "varName ?value value value ...?"
  set ::nsf::parametersyntax(::set) "varName ?value?"
  set ::nsf::parametersyntax(::set) "varName ?value?"
  set ::nsf::parametersyntax(::subst) "?-nobackslashes? ?-nocommands? ?-novariables? string"
  set ::nsf::parametersyntax(::trace) "option ?arg arg ...?"
  set ::nsf::parametersyntax(::unset) "?-nocomplain? ?--? ?name name name ...?"

  set ::nsf::parametersyntax(::nsf::classes::xotcl::Object::invar) "?expr?"
  set ::nsf::parametersyntax(::nsf::classes::xotcl::Class::instinvar) "?expr?"
  set ::nsf::parametersyntax(::nsf::classes::xotcl::Object::parametercmd) "name"
  set ::nsf::parametersyntax(::nsf::classes::xotcl::Class::instparametercmd) "name"
  set ::nsf::parametersyntax(::nsf::classes::xotcl::Class::slots) "cmds"

  # slots
  set ::nsf::parametersyntax(::nsf::classes::xotcl::Object::class) "?class?"

  set value "?classes?|?add class?|?delete class?"
  set ::nsf::parametersyntax(::nsf::classes::xotcl::Object::mixin) $value
  set ::nsf::parametersyntax(::nsf::classes::xotcl::Class::instmixin) $value
  set ::nsf::parametersyntax(::nsf::classes::xotcl::Class::superclass) $value

  set value "?filters?|?add filter?|?delete filter?"
  set ::nsf::parametersyntax(::nsf::classes::xotcl::Object::filter) $value
  set ::nsf::parametersyntax(::nsf::classes::xotcl::Class::instfilter) $value
  unset value

}

if {[::nsf::configure debug] > 1} {
  foreach ns {::xotcl} {
    puts "vars of $ns: [info vars ${ns}::*]"
    puts stderr "$ns exports: [namespace eval $ns {lsort [namespace export]}]"
  }
  puts stderr "======= XOTcl $::xotcl::version$::xotcl::patchlevel loaded"
}
