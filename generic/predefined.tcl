namespace eval ::nx {
  #
  # By setting the variable bootstrap, we can check later, whether we
  # are in bootstrapping mode
  #
  set bootstrap 1

  #namespace path ::xotcl

  #
  # First create the ::nx object system. 
  #
  ::nx::core::createobjectsystem ::nx::Object ::nx::Class {
    -class.alloc alloc 
    -class.create create
    -class.dealloc dealloc
    -class.recreate recreate 
    -class.requireobject __unknown
    -object.configure configure
    -object.defaultmethod defaultmethod 
    -object.destroy destroy 
    -object.init init 
    -object.move move 
    -object.objectparameter objectparameter 
    -object.residualargs residualargs
    -object.unknown unknown
  }

  #
  # get frequenly used primitiva into the ::nx::core namespace
  #
  namespace eval ::nx::core {
    namespace export next current my is relation interp
  }
  
  namespace import ::nx::core::next ::nx::core::current

  #
  # provide the standard command set for ::nx::Object
  #
  foreach cmd [info command ::nx::core::cmd::Object::*] {
    set cmdName [namespace tail $cmd]
    if {$cmdName in [list "exists" "instvar"]} continue
    ::nx::core::alias Object $cmdName $cmd 
  }
  
  # provide ::eval as method for ::nx::Object
  ::nx::core::alias Object eval -nonleaf ::eval

  # provide the standard command set for Class
  foreach cmd [info command ::nx::core::cmd::Class::*] {
    set cmdName [namespace tail $cmd]
    ::nx::core::alias Class $cmdName $cmd 
  }

  # set a few aliases as protected
  # "__next", if defined, should be added as well
  foreach cmd [list cleanup noinit residualargs uplevel upvar] {
    ::nx::core::methodproperty Object $cmd protected 1
  }

  foreach cmd [list recreate] {
    ::nx::core::methodproperty Class $cmd protected 1
  }
  # TODO: info methods shows finally "slots" and "slot". Wanted?

  # protect some methods against redefinition
  ::nx::core::methodproperty Object destroy redefine-protected true
  ::nx::core::methodproperty Class  alloc   redefine-protected true
  ::nx::core::methodproperty Class  dealloc redefine-protected true
  ::nx::core::methodproperty Class  create  redefine-protected true
  
  # define method "method" for Class and Object

  # @method ::nx::Class#method 
  #
  # Defines a per-class method, similarly to Tcl specifying
  # {{{procs}}}. Optionally assertions may be specified by two
  # additional arguments. Therefore, to specify only post-assertions
  # an empty pre-assertion list must be given. All assertions are a
  # list of ordinary Tcl {{{expr}}} statements.  When {{{method}}} is
  # called with an empty argument list and an empty body, the
  # specified method is deleted.
  # {{{
  #	Class create AClass {
  #		:method foo args {;}
  #	}
  #
  #	AClass create anInstance
  #	anInstance foo; # invokes "foo"
  # }}}
  #
  # @param name The method name
  # @param arguments:list A list specifying non-positional and positional parameters
  # @param body The script which forms the method body
  # @param preAssertion Optional assertions that must hold before the proc executes
  # @param postAssertion Optional assertions that must hold after the proc executes

  ::nx::core::method Class method {
    name arguments body -precondition -postcondition
  } {
    set conditions [list]
    if {[info exists precondition]}  {lappend conditions -precondition  $precondition}
    if {[info exists postcondition]} {lappend conditions -postcondition $postcondition}
    ::nx::core::method [::nx::core::current object] $name $arguments $body {*}$conditions
  }

  # @method ::nx::Object#method 
  #
  # Defines a per-object method, similarly to Tcl specifying
  # {{{procs}}}.  Optionally assertions may be specified by two
  # additional arguments. Therefore, to specify only post-assertions
  # an empty pre-assertion list must be given. All assertions are a
  # list of ordinary Tcl {{{expr}}} statements.  When {{{method}}} is
  # called with an empty argument list and an empty body, the
  # specified method is deleted.
  # {{{
  # 	Object create anObject {
  #   		:method foo args {;}
  # 	}
  # 	anObject foo; # invokes "foo"
  # }}}
  #
  # @param name The method name
  # @param arguments:list A list specifying non-positional and positional parameters
  # @param body The script which forms the method body
  # @param preAssertion Optional assertions that must hold before the proc executes
  # @param postAssertion Optional assertions that must hold after the proc executes
  ::nx::core::method Object method {
    name arguments body -precondition -postcondition
  } {
    set conditions [list]
    if {[info exists precondition]}  {lappend conditions -precondition  $precondition}
    if {[info exists postcondition]} {lappend conditions -postcondition $postcondition}
    ::nx::core::method [::nx::core::current object] -per-object $name $arguments $body {*}$conditions
  }

  # define method modifiers "object", "public" and "protected"
  Class eval {

    # method-modifier for object specific methos
    :method object {what args} {
      if {$what in [list "alias" "attribute" "forward" "method" "setter"]} {
        return [::nx::core::dispatch [::nx::core::current object] ::nx::core::classes::nx::Object::$what {*}$args]
      }
      if {$what in [list "info"]} {
        return [::nx::objectInfo [lindex $args 0] [::nx::core::current object] {*}[lrange $args 1 end]]
      }
      if {$what in [list "filter" "mixin"]} {
        return [:object-$what {*}$args]
      }
      if {$what in [list "filterguard" "mixinguard"]} {
        return [::nx::core::dispatch [::nx::core::current object] ::nx::core::cmd::Object::$what {*}$args]
      }
    }

    # define unknown handler for class
    :method unknown {m args} {
      error "Method '$m' unknown for [::nx::core::current object].\
	Consider '[::nx::core::current object] create $m $args' instead of '[::nx::core::current object] $m $args'"
    }
    # protected is not jet defined
    ::nx::core::methodproperty [::nx::core::current object] unknown protected 1
  }


  Object eval {

    # method modifier "public"
    :method public {args} {
      set p [lsearch -regexp $args {^(method|alias|attribute|forward|setter)$}]
      if {$p == -1} {error "$args is not a method defining method"}
      set r [{*}:$args]
      ::nx::core::methodproperty [::nx::core::current object] $r protected false
      return $r
    }

    # method modifier "protected"
    :method protected {args} {
      set p [lsearch -regexp $args {^(method|alias|attribute|forward|setter)$}]
      if {$p == -1} {error "$args is not a method defining command"}
      set r [{*}:$args]
      ::nx::core::methodproperty [::nx::core::current object] $r [::nx::core::current method] true
      return $r
    }

    # unknown handler for Object
    :protected method unknown {m args} {
      if {![::nx::core::current isnext]} {
        error "[::nx::core::current object]: unable to dispatch method '$m'"
      }
    }
    
    # "init" must exist on Object. per default it is empty.
    :protected method init args {}

    # this method is called on calls to object without a specified method
    :protected method defaultmethod {} {::nx::core::current object}

    # provide a placeholder for the bootup process. The real definition
    # is based on slots, which are not available at this point.
    :protected method objectparameter {} {;}
  }

  # define forward methods

  # @method ::nx::Object#forward
  #
  # Register a per-object method (similar to a {{{proc}}}) for
  # forward-delegating calls to a callee (target Tcl command, other
  # object). When the forwarder method is called, the actual arguments
  # of the invocation are appended to the specified arguments. In
  # callee an arguments certain substitutions can take place:
  #
  # {{{%proc}}} substituted by name of the forwarder method
  #
  # {{{%self}}} substitute by name of the object
  #
  # {{{%1}}} substitute by first argument of the invocation
  #
  # {{{ {%@POS value} }}} substitute the specified value in the
  # argument list on position POS, where POS can be a positive or
  # negative integer or end. Positive integers specify the position
  # from the begin of the list, while negative integer specify the
  # position from the end.
  #
  # {{{ {%argclindex LIST} }}} take the nth argument of the specified
  # list as substitution value, where n is the number of arguments
  # from the invocation.
  #
  # {{{%%}}} a single percent.
  #
  # {{{%Tcl-command}}} command to be executed; substituted by result.
  #
  # Additionally each argument can be prefixed by the positional prefix
  # %@POS (note the delimiting space at the end) that can be used to
  # specify an explicit position. POS can be a positive or negative
  # integer or the word end. The positional arguments are evaluated from
  # left to right and should be used in ascending order. 
  #
  # @param name The name of the delegating or forward method
  # @param -objscope:optional Causes the target to be evaluated in the scope of the object.
  # @param -methodprefix Prepends the specified prefix to the second argument of the invocation.
  # @param -default Is used for default method names (only in connection with %1)
  # @param -earlybinding Look up the function pointer of the called Tcl command at definition time of the forwarder instead of invocation time. This option should only be used for calling C-implemented Tcl commands, no scripted procs
  # @param -verbose Print the substituted command to stderr before executing
  # @param callee
  # @param args
  ::nx::core::forward Object forward ::nx::core::forward %self -per-object
  #set ::nx::core::signature(::nx::Object-method-forward) {(methodName) obj forward name ?-default default? ?-earlybinding? ?-methodprefix name? ?-objscope? ?-onerror proc? ?-verbose? target ?args?}

  # @method ::nx::Class#forward
  #
  # Register a per-class method (similar to a {{{proc}}}) for
  # forward-delegating calls to a callee (target Tcl command, other
  # object). When the forwarder method is called on an instance of the
  # class, the actual arguments of the invocation are appended to the
  # specified arguments. In callee an arguments certain substitutions
  # can take place:
  #
  # {{{%proc}}} substituted by name of the forwarder method
  #
  # {{{%self}}} substitute by name of the object
  #
  # {{{%1}}} substitute by first argument of the invocation
  #
  # {{{ {%@POS value} }}} substitute the specified value in the
  # argument list on position POS, where POS can be a positive or
  # negative integer or end. Positive integers specify the position
  # from the begin of the list, while negative integer specify the
  # position from the end.
  #
  # {{{ {%argclindex LIST} }}} take the nth argument of the specified
  # list as substitution value, where n is the number of arguments
  # from the invocation.
  #
  # {{{%%}}} a single percent.
  #
  # {{{%Tcl-command}}} command to be executed; substituted by result.
  #
  # Additionally each argument can be prefixed by the positional prefix
  # %@POS (note the delimiting space at the end) that can be used to
  # specify an explicit position. POS can be a positive or negative
  # integer or the word end. The positional arguments are evaluated from
  # left to right and should be used in ascending order. 
  #
  # @param name The name of the delegating or forward method
  # @param -objscope:optional Causes the target to be evaluated in the scope of the object.
  # @param -methodprefix Prepends the specified prefix to the second argument of the invocation.
  # @param -default Is used for default method names (only in connection with %1)
  # @param -earlybinding Look up the function pointer of the called Tcl command at definition time of the forwarder instead of invocation time. This option should only be used for calling C-implemented Tcl commands, no scripted procs
  # @param -verbose Print the substituted command to stderr before executing
  # @param callee
  # @param args
  ::nx::core::forward Class forward ::nx::core::forward %self

  # The method __unknown is called in cases, where we try to resolve
  # an unkown class. one could define a custom resolver with this name
  # to load the class on the fly. After the call to __unknown, XOTcl
  # tries to resolve the class again. This meachnism is used e.g. by
  # the ::ttrace mechanism for partial loading by Zoran.
  #
  Class protected object method __unknown {name} {}

  # Add alias methods. cmdName for XOTcl method can be added via
  #   [... info method name <methodName>]
  #
  # -nonleaf and -objscope make only sense for c-defined cmds,
  # -objscope implies -nonleaf
  #
  Object public method alias {-nonleaf:switch -objscope:switch methodName cmd} {
    ::nx::core::alias [::nx::core::current object] -per-object $methodName \
        {*}[expr {${objscope} ? "-objscope" : ""}] \
        {*}[expr {${nonleaf} ? "-nonleaf" : ""}] \
        $cmd
  }
  Class public method alias {-nonleaf:switch -objscope:switch methodName cmd} {
    ::nx::core::alias [::nx::core::current object] $methodName \
        {*}[expr {${objscope} ? "-objscope" : ""}] \
        {*}[expr {${nonleaf} ? "-nonleaf" : ""}] \
        $cmd
  }

  # Add setter methods. 
  #  
  Object public method setter {methodName} {
    ::nx::core::setter [::nx::core::current object] -per-object $methodName
  }
  Class public method setter {methodName} {
    ::nx::core::setter [::nx::core::current object] $methodName
  }

  ########################
  # Info definition
  ########################
  Object create ::nx::objectInfo
  Object create ::nx::classInfo

  #
  # It would be nice to do here "objectInfo configure {alias ..}", but
  # we have no working objectparameter yet due to bootstrapping
  #
  objectInfo eval {
    :alias is ::nx::core::objectproperty

    # info info
    :public method info {obj} {
      set methods [list]
      foreach name [::nx::core::cmd::ObjectInfo::methods [::nx::core::current object]] {
        if {$name eq "unknown"} continue
        lappend methods $name
      }
      return "valid options are: [join [lsort $methods] {, }]"
    }

    :method unknown {method obj args} {
      error "[::nx::core::current object] unknown info option \"$method\"; [$obj info info]"
    }
  }

  classInfo eval {
    :alias is ::nx::core::objectproperty
    :alias classparent ::nx::core::cmd::ObjectInfo::parent
    :alias classchildren ::nx::core::cmd::ObjectInfo::children
    :alias info [::nx::core::cmd::ObjectInfo::method objectInfo name info]
    :alias unknown [::nx::core::cmd::ObjectInfo::method objectInfo name info]
  }

  foreach cmd [info command ::nx::core::cmd::ObjectInfo::*] {
    ::nx::core::alias ::nx::objectInfo [namespace tail $cmd] $cmd
    ::nx::core::alias ::nx::classInfo [namespace tail $cmd] $cmd
  }
  foreach cmd [info command ::nx::core::cmd::ClassInfo::*] {
    set cmdName [namespace tail $cmd]
    if {$cmdName in [list "object-mixin-of" "class-mixin-of"]} continue
    ::nx::core::alias ::nx::classInfo $cmdName $cmd
  }
  unset cmd

  # register method "info" on Object and Class
  Object forward info -onerror ::nx::core::infoError ::nx::objectInfo %1 {%@2 %self}
  Class forward  info -onerror ::nx::core::infoError ::nx::classInfo %1 {%@2 %self}

  proc ::nx::core::infoError msg {
    #puts stderr "INFO ERROR: <$msg>\n$::errorInfo"
    regsub -all " <object>" $msg "" msg
    regsub -all " <class>" $msg "" msg
    regsub {\"} $msg "\"info " msg
    error $msg ""
  }
  
  #
  # definition of "abstract method foo ...."
  #
  Object method abstract {methtype -per-object:switch methname arglist} {
    if {$methtype ne "method"} {
      error "invalid method type '$methtype', must be 'method'"
    }
    set body "
      if {!\[::nx::core::current isnextcall\]} {
        error \"Abstract method $methname $arglist called\"
      } else {::nx::core::next}
    "
    if {${per-object}} {
      :method -per-object $methname $arglist $body
    }  else {
      :method $methname $arglist $body
    }
  }

  #
  # exit handlers
  #
  proc ::nx::core::unsetExitHandler {} {
    proc ::nx::core::__exitHandler {} {
      # clients should append exit handlers to this proc body
    }
  }
  proc ::nx::core::setExitHandler {newbody} {::proc ::nx::core::__exitHandler {} $newbody}
  proc ::nx::core::getExitHandler {} {::info body ::nx::core::__exitHandler}
  # initialize exit handler
  ::nx::core::unsetExitHandler
 
}


########################################
# Slot definitions
########################################
namespace eval ::nx {
  #
  # We are in bootstrap code; we cannot use slots/parameter to define
  # slots, so the code is a little low level. After the defintion of
  # the slots, we can use slot-based code such as "-parameter" or
  # "objectparameter".
  #
  ::nx::Class create ::nx::MetaSlot
  ::nx::core::relation ::nx::MetaSlot superclass ::nx::Class

  ::nx::MetaSlot public method slotName {name baseObject} {
    # Create slot parent object if needed
    set slotParent ${baseObject}::slot
    if {![::nx::core::objectproperty ${slotParent} object]} {
      ::nx::Object create ${slotParent}
    }
    return ${slotParent}::$name
  }

  ::nx::MetaSlot method createFromParameterSyntax {
    target -per-object:switch 
    {-initblock ""} 
    value default:optional
  } {
    set opts [list]
    set colonPos [string first : $value]
    if {$colonPos == -1} {
      set name $value
    } else {
      set properties [string range $value [expr {$colonPos+1}] end]
      set name [string range $value 0 [expr {$colonPos -1}]]
      foreach property [split $properties ,] {
        if {$property eq "required"} {
          lappend opts -required 1
        } elseif {$property eq "multivalued"} {
          lappend opts -multivalued 1
        } elseif {[string match type=* $property]} {
          set type [string range $property 5 end]
          if {![string match ::* $type]} {set type ::$type}
        } elseif {[string match arg=* $property]} {
          set argument [string range $property 4 end]
          lappend opts -arg $argument
        } else {
          set type $property
        }
      }
    }
    if {[info exists type]} {
      lappend opts -type $type
    }

    if {[info exists default]} {
      lappend opts -default $default
    }
    if {${per-object}} {
      lappend opts -per-object true
      set info ObjectInfo
    } else {
      set info ClassInfo
    }

    :create [:slotName $name $target] {*}$opts $initblock
    return [::nx::core::cmd::${info}::method $target name $name]
  }

  # @object ::nx::Slot
  #
  # A slot is a meta-object that manages property changes of
  # objects. A property is either an attribute or a role taken by an
  # object in an inter-object relation (e.g., in system slots). The
  # predefined system slots are {{{class}}}, {{{superclass}}},
  # {{{mixin}}}, and {{{filter}}}. These slots appear as methods of
  # {{@object ::nx::Object}} or {{@object ::nx::Class}}. The slots
  # provide a common getter and setter interface. Every multivalued
  # slot provides e.g. a method {{{add}}} to append a value to the
  # list of values, and a method {{{delete}}} which removes it.
  #
  # @superclass ::nx::doc::entities::object::nx::Object
  ::nx::MetaSlot create ::nx::Slot

  # @object ::nx::ObjectParameterSlot
  #
  # @superclass ::nx::doc::entities::object::nx::Slot
  ::nx::MetaSlot create ::nx::ObjectParameterSlot
  ::nx::core::relation ::nx::ObjectParameterSlot superclass ::nx::Slot
  
  ::nx::MetaSlot create ::nx::MethodParameterSlot
  ::nx::core::relation ::nx::MethodParameterSlot superclass ::nx::Slot

  # create an object for dispatching
  ::nx::MethodParameterSlot create ::nx::methodParameterSlot
  
  # use low level interface for defining slot values. Normally, this is
  # done via slot objects, which are defined later.

  proc createBootstrapAttributeSlots {class definitions} {
    foreach att $definitions {
      if {[llength $att]>1} {foreach {att default} $att break}
      set slotObj [::nx::ObjectParameterSlot slotName $att $class] 
      ::nx::ObjectParameterSlot create $slotObj
      if {[info exists default]} {
        ::nx::core::setvar $slotObj default $default
        unset default
      }
      ::nx::core::setter $class $att
    }
    
    #
    # Perform a second round to set default values for already defined
    # objects.
    #
    foreach att $definitions {
      if {[llength $att]>1} {foreach {att default} $att break}
      if {[info exists default]} {

        # checking subclasses is not required during bootstrap
        foreach i [::nx::core::cmd::ClassInfo::instances $class] {
          if {![::nx::core::existsvar $i $att]} {
            if {[string match {*\[*\]*} $default]} {
              set value [::nx::core::dispatch $i -objscope ::eval subst $default]
            } else {
	      set value $default
	    }
            ::nx::core::setvar $i $att $value
          }
        }
        unset default
      }
    }

    #puts stderr "Bootstrapslot for $class calls __invalidateobjectparameter"
    $class __invalidateobjectparameter
  }

  ############################################
  # Define slots for slots
  ############################################

  # @param ::nx::Slot#name
  #
  # Name of the slot which can be used to access the slot from an object

  # @param ::nx::Slot#multivalued
  #
  # Boolean value for specifying single or multiple values (lists)

  # @param ::nx::Slot#required
  #
  # Denotes whether a value must be provided

  # @param ::nx::Slot#default
  #
  # Allows you to define a default value (to be set upon object creation)

  # @param ::nx::Slot#type
  #
  # You may specify a type constraint on the value range to managed by the slot

  createBootstrapAttributeSlots ::nx::Slot {
    {name}
    {multivalued false}
    {required false}
    default
    type
  }

  # @param ::nx::ObjectParameterSlot#name
  #
  # Name of the slot which can be used to access the slot from an
  # object. It defaults to unqualified name of an instance.

  # @param ::nx::ObjectParameterSlot#methodname
  #
  # The name of the accessor methods to be registed on behalf of the
  # slot object with its domains can vary from the slot name.

  # @param ::nx::ObjectParameterSlot#domain
  #
  # The domain (object or class) of a slot on which it can be used

  # @param ::nx::ObjectParameterSlot#defaultmethods
  #
  # A list of two elements for specifying which methods are called per
  # default, when no slot method is explicitly specified in a call.

  # @param ::nx::ObjectParameterSlot#manager
  #
  # The manager object of the slot (per default, the slot object takes
  # this role, i.e. {{{[self]}}})

  # @param ::nx::ObjectParameterSlot#per-object
  #
  # If set to {{{true}}}, the accessor methods are registered with the
  # domain object scope only. It defaults to {{{false}}}.

  createBootstrapAttributeSlots ::nx::ObjectParameterSlot {
    {name "[namespace tail [::nx::core::current object]]"}
    {methodname}
    {domain "[lindex [regexp -inline {^(.*)::slot::[^:]+$} [::nx::core::current object]] 1]"}
    {defaultmethods {get assign}}
    {manager "[::nx::core::current object]"}
    {per-object false}
  }
  # maybe add the following slots at some later time here
  #   initcmd
  #   valuecmd
  #   valuechangedcmd
  
  ::nx::core::alias ::nx::ObjectParameterSlot get ::nx::core::setvar
  ::nx::core::alias ::nx::ObjectParameterSlot assign ::nx::core::setvar
  
  ::nx::ObjectParameterSlot public method add {obj prop value {pos 0}} {
    if {![set :multivalued]} {
      error "Property $prop of [set :domain]->$obj ist not multivalued"
    }
    if {[::nx::core::existsvar $obj $prop]} {
      ::nx::core::setvar $obj $prop [linsert [::nx::core::setvar $obj $prop] $pos $value]
    } else {
      ::nx::core::setvar $obj $prop [list $value]
    }
  }
  ::nx::ObjectParameterSlot public method delete {-nocomplain:switch obj prop value} {
    set old [::nx::core::setvar $obj $prop]
    set p [lsearch -glob $old $value]
    if {$p>-1} {::nx::core::setvar $obj $prop [lreplace $old $p $p]} else {
      error "$value is not a $prop of $obj (valid are: $old)"
    }
  }
  
  ::nx::ObjectParameterSlot method unknown {method args} {
    set methods [list]
    foreach m [:info callable] {
      if {[::nx::Object info callable $m] ne ""} continue
      if {[string match __* $m]} continue
      lappend methods $m
    }
    error "Method '$method' unknown for slot [::nx::core::current object]; valid are: {[lsort $methods]}"
  }
  
  ::nx::ObjectParameterSlot public method destroy {} {
    if {${:domain} ne "" && [::nx::core::objectproperty ${:domain} class]} {
      ${:domain} __invalidateobjectparameter
    }
    ::nx::core::next
  }
  
  ::nx::ObjectParameterSlot protected method init {args} {
    if {${:domain} eq ""} {
      set :domain [::nx::core::current callingobject]
    }
    if {${:domain} ne ""} {
      if {![info exists :methodname]} {
        set :methodname ${:name}
      }
      if {[::nx::core::objectproperty ${:domain} class]} {
        ${:domain} __invalidateobjectparameter
      } 
      if {${:per-object} && [info exists :default] } {
        ::nx::core::setvar ${:domain} ${:name} ${:default}
      }
      set cl [expr {${:per-object} ? "Object" : "Class"}]
      #puts stderr "Slot [::nx::core::current object] init, forwarder on ${:domain}"
      ::nx::core::forward ${:domain} ${:name} \
          ${:manager} \
          [list %1 [${:manager} defaultmethods]] %self \
          ${:methodname}
    }
  }

  #################################################################
  # We have no working objectparameter yet, since it requires a
  # minimal slot infrastructure to build object parameters from
  # slots. The above definitions should be sufficient. We provide the
  # definition here before we refine the slot definitions.
  # 
  # Invalidate previously defined object parameter.
  ::nx::MetaSlot __invalidateobjectparameter

  # Provide the a slot based mechanism for building an object
  # configuration interface from slot definitions
  ::nx::ObjectParameterSlot method toParameterSyntax {{name:substdefault ${:name}}} {
    set objparamdefinition $name
    set methodparamdefinition ""
    set objopts [list]
    set methodopts [list]
    set type ""
    if {[info exists :required] && ${:required}} {
      lappend objopts required
      lappend methodopts required
    }
    if {[info exists :type]} {
      if {[string match ::* ${:type}]} {
	set type [expr {[::nx::core::objectproperty ${:type} metaclass] ? "class" : "object"}]
        lappend objopts type=${:type}
        lappend methodopts type=${:type}
      } else {
        set type ${:type}
      }
    }
    # TODO: remove multivalued check on relations by handling multivalued
    # not in relation, but in the converters
    if {[info exists :multivalued] && ${:multivalued}} {
      if {!([info exists :type] && ${:type} eq "relation")} {
        lappend objopts multivalued
      } else {
        #puts stderr "ignore multivalued for $name in relation"
      }
    }
    if {[info exists :arg]} {
      set prefix [expr {$type eq "object" || $type eq "class" ? "type" : "arg"}]
      lappend objopts $prefix=${:arg}
      lappend methodopts $prefix=${:arg}
    }
    if {[info exists :default]} {
      set arg ${:default}
      # deactivated for now: || [string first {$} $arg] > -1
      if {[string match {*\[*\]*} $arg] 
          && $type ne "substdefault"} {
        lappend objopts substdefault
      }
    } elseif {[info exists :initcmd]} {
      set arg ${:initcmd}
      lappend objopts initcmd
    }
    if {[info exists :methodname]} {
      if {${:methodname} ne ${:name}} {
        lappend objopts arg=${:methodname}
        lappend methodopts arg=${:methodname}
        #puts stderr "..... setting arg for methodname: [::nx::core::current object] has arg arg=${:methodname}"
      }
    }
    if {$type ne ""} {
      set objopts [linsert $objopts 0 $type]
      # Never add "substdefault" to methodopts, since these are for
      # provided values, not for defaults.
      if {$type ne "substdefault"} {set methodopts [linsert $methodopts 0 $type]}
    }
    lappend objopts slot=[::nx::core::current object]

    if {[llength $objopts] > 0} {
      append objparamdefinition :[join $objopts ,]
    }
    if {[llength $methodopts] > 0} {
      set methodparamdefinition [join $methodopts ,]
    }
    if {[info exists arg]} {
      lappend objparamdefinition $arg
    }
    #puts stderr "[::nx::core::current method] ${name} returns [list oparam $objparamdefinition mparam $methodparamdefinition]"
    return [list oparam $objparamdefinition mparam $methodparamdefinition]
  }

  
  proc ::nx::core::parametersFromSlots {obj} {
    set parameterdefinitions [list]
    foreach slot [::nx::objectInfo slotobjects $obj] {
      # Skip some slots for xotcl; 
      # TODO: maybe different parameterFromSlots for xotcl?
      if {[::nx::core::objectproperty ::xotcl::Object class] 
	  && [::nx::core::objectproperty $obj type ::xotcl::Object] && 
          ([$slot name] eq "mixin" || [$slot name] eq "filter")
      } continue
      array set "" [$slot toParameterSyntax]
      lappend parameterdefinitions -$(oparam)
    }
    return $parameterdefinitions
  }

  # @method ::nx::Object#objectparameter
  ::nx::Object protected method objectparameter {{lastparameter __initcmd:initcmd,optional}} {
    #puts stderr "... objectparameter [::nx::core::current object]"
    set parameterdefinitions [::nx::core::parametersFromSlots [::nx::core::current object]]
    if {[::nx::core::objectproperty [::nx::core::current object] class]} {
      lappend parameterdefinitions -parameter:method,optional
    }
    lappend parameterdefinitions \
        -noinit:method,optional,noarg \
        -volatile:method,optional,noarg \
        {*}$lastparameter
    #puts stderr "*** parameter definition for [::nx::core::current object]: $parameterdefinitions"
    return $parameterdefinitions
  }


  ############################################
  #  RelationSlot
  ############################################
  ::nx::MetaSlot create ::nx::RelationSlot
  createBootstrapAttributeSlots ::nx::RelationSlot {
    {multivalued true}
    {type relation}
    {elementtype ::nx::Class}
  }
  ::nx::core::relation ::nx::RelationSlot superclass ::nx::ObjectParameterSlot
  ::nx::core::alias ::nx::RelationSlot assign ::nx::core::relation

  ::nx::RelationSlot protected method init {} {
    if {${:type} ne "relation"} {
      error "RelationSlot requires type == \"relation\""
    }
    ::nx::core::next
  }
  ::nx::RelationSlot protected method delete_value {obj prop old value} {
    if {[string first * $value] > -1 || [string first \[ $value] > -1} {
      # value contains globbing meta characters
      if {${:elementtype} ne "" && ![string match ::* $value]} {
        # prefix glob pattern with ::, since all object names have leading ::
        set value ::$value
      }
      return [lsearch -all -not -glob -inline $old $value]
    } elseif {${:elementtype} ne ""} {
      # value contains no globbing meta characters, but elementtype is given
      if {[string first :: $value] == -1} {
	# get fully qualified name
        if {![::nx::core::objectproperty $value object]} {
          error "$value does not appear to be an object"
        }
        set value [::nx::core::dispatch $value -objscope ::nx::core::current object]
      }
      if {![::nx::core::objectproperty ${:elementtype} class]} {
        error "$value does not appear to be of type ${:elementtype}"
      }
    }
    set p [lsearch -exact $old $value]
    if {$p > -1} {
      return [lreplace $old $p $p]
    } else {
      error "$value is not a $prop of $obj (valid are: $old)"
    }
  }
 
  ::nx::RelationSlot public method delete {-nocomplain:switch obj prop value} {
    #puts stderr RelationSlot-delete-[::nx::core::current args]
    $obj $prop [:delete_value $obj $prop [$obj info $prop] $value]
  }
  
  ::nx::RelationSlot public method get {obj prop} {
    ::nx::core::relation $obj $prop
  }

  ::nx::RelationSlot public method add {obj prop value {pos 0}} {
    if {![set :multivalued]} {
      error "Property $prop of ${:domain}->$obj ist not multivalued"
    }
    set oldSetting [::nx::core::relation $obj $prop]
    # use uplevel to avoid namespace surprises
    uplevel [list ::nx::core::relation $obj $prop [linsert $oldSetting $pos $value]]
  }
  ::nx::RelationSlot public method delete {-nocomplain:switch obj prop value} {
    uplevel [list ::nx::core::relation $obj $prop [:delete_value $obj $prop [::nx::core::relation $obj $prop] $value]]
  }

 
  ############################################
  # system slots
  ############################################
  proc ::nx::core::register_system_slots {os} {
    ${os}::Object alloc ${os}::Class::slot
    ${os}::Object alloc ${os}::Object::slot

    # @param ::nx::Class#superclass
    #
    # Specifies superclasses for a given class. As a setter,
    # {{{superclass}}} changes the list of superclasses. When used as
    # a getter, the method returns the current superclasses.
    #
    # @return :list If called as a getter (without arguments),
    # {{{superclass}}} returns the current superclasses of the object
    ::nx::RelationSlot create ${os}::Class::slot::superclass
    ::nx::core::alias         ${os}::Class::slot::superclass assign ::nx::core::relation

    # @param ::nx::Object#class
    #
    # Sets or retrieves the class of an object. When {{{class}}} is
    # called without arguments, it returns the current class of the
    # object.
    #
    # @return If called as a getter (without arguments), {{{class}}} returns the current class of the object
    ::nx::RelationSlot create ${os}::Object::slot::class -multivalued false
    ::nx::core::alias         ${os}::Object::slot::class assign ::nx::core::relation

    # @param ::nx::Object#mixin
    #
    # As a setter, {{{mixin}}} specifies a list of mixins to
    # set. Every mixin must be an existing class. In getter mode, you
    # can retrieve the list of mixins active for the given object.
    #
    # @return :list If called as a getter (without arguments), {{{mixin}}} returns the list of current mixin classes registered with the object
    ::nx::RelationSlot create ${os}::Object::slot::mixin -methodname object-mixin    

    # @param ::nx::Object#filter
    #
    # In its setter mode, {{{filter}}} allows you to register methods
    # as per-object filters. Every filter must be an existing method
    # in the scope of the object. When acting as a getter, you can
    # retrieve the list of filter methods active for the given object.
    #
    # @return :list If called as a getter (without arguments),
    # {{{filter}}} returns the list of current filters
    # registered with the object
    ::nx::RelationSlot create ${os}::Object::slot::filter -elementtype ""
    
    # @param ::nx::Class#mixin
    #
    # As a setter, {{{mixin}}} specifies a list of mixins to set for
    # the class. Every mixin must be an existing class. In getter
    # mode, you can retrieve the list of mixins active for the given
    # class.
    #
    # @return :list If called as a getter (without arguments), {{{mixin}}} returns the list of current mixin classes registered with the class
    ::nx::RelationSlot create ${os}::Class::slot::mixin -methodname class-mixin
    
    # @param ::nx::Class#filter
    #
    # In its setter mode, {{{filter}}} allows you to register methods
    # as per-class filters. Every filter must be an existing method
    # in the scope of the class. When acting as a getter, you can
    # retrieve the list of filter methods active for the given class.
    #
    # @return :list If called as a getter (without arguments),
    # {{{filter}}} returns the list of current filters
    # registered with the class
    ::nx::RelationSlot create ${os}::Class::slot::filter -elementtype "" \
        -methodname class-filter
    
    # Create two conveniance slots to allow configuration of 
    # object-slots for classes via object-mixin
    ::nx::RelationSlot create ${os}::Class::slot::object-mixin 
    ::nx::RelationSlot create ${os}::Class::slot::object-filter -elementtype ""
  }

  ::nx::core::register_system_slots ::nx
  proc ::nx::core::register_system_slots {} {}

  
  ############################################
  # Attribute slots
  ############################################
  ::nx::MetaSlot __invalidateobjectparameter
  
  # @object ::nx::Attribute
  #
  # Attribute slots are used to manage the access, mutation, and
  # querying of instance variables. One defines Attribute slots 
  # for objects and classes usually via the helper method 
  # {{@method ::nx::Object class attribute}} 
  # **** TODO STEFAN, kein Link? GEPLANT? MIT 2 GESCHWEIFTEN KLAMMER UM SALARY GIBT ES EINEN LAUFZEITFEHLER??? ********
  # The following example defines a class with
  # three attribute slots. The attribute {salary} has 
  # a default of {0}, the attribute {projects} has the
  # empty list as default and is defined as multivalued.
  # {{{
  #   Class create Person {
  #      :attribute name
  #      :attribute {salary:integer 0}
  #      :attribute {projects:multivalued ""} {
  #         set :incremental true
  #      }
  #   }
  # }}}
  #
  # @param incremental A boolean value, only useful for multivalued slots. When set, one can add/delete incrementally values to the multivalued set (e.g., through an incremental {{{add}}})
  # @param valuecmd A Tcl command to be executed whenever the managed object variable is read
  # @param valuechangedcmd A Tcl command to be executed whenever the value of the managed object variable changes
  # @param arg
  # @superclass ::nx::doc::entities::object::nx::ObjectParameterSlot
  ::nx::MetaSlot create ::nx::Attribute -superclass ::nx::ObjectParameterSlot

  createBootstrapAttributeSlots ::nx::Attribute {
    {value_check once}
    incremental
    initcmd
    valuecmd
    valuechangedcmd
    arg
  }

  ::nx::Attribute method __default_from_cmd {obj cmd var sub op} {
    #puts "GETVAR [::nx::core::current method] obj=$obj cmd=$cmd, var=$var, op=$op"
    $obj trace remove variable $var $op [list [::nx::core::current object] [::nx::core::current method] $obj $cmd]
    ::nx::core::setvar $obj $var [$obj eval $cmd]
  }
  ::nx::Attribute method __value_from_cmd {obj cmd var sub op} {
    #puts "GETVAR [::nx::core::current method] obj=$obj cmd=$cmd, var=$var, op=$op"
    ::nx::core::setvar $obj $var [$obj eval $cmd]
  }
  ::nx::Attribute method __value_changed_cmd {obj cmd var sub op} {
    # puts stderr "**************************"
    # puts "valuechanged obj=$obj cmd=$cmd, var=$var, op=$op, ...\n$obj exists $var -> [::nx::core::setvar $obj $var]"
    eval $cmd
  }
  ::nx::Attribute protected method init {} {
    ::nx::core::next ;# do first ordinary slot initialization
    # there might be already default values registered on the class
    set __initcmd ""
    if {[info exists :default]} {
    } elseif [info exists :initcmd] {
      append __initcmd ":trace add variable [list ${:name}] read \
	\[list [::nx::core::current object] __default_from_cmd \[::nx::core::current object\] [list [set :initcmd]]\]\n"
    } elseif [info exists :valuecmd] {
      append __initcmd ":trace add variable [list ${:name}] read \
	\[list [::nx::core::current object] __value_from_cmd \[::nx::core::current object\] [list [set :valuecmd]]\]"
    }
    array set "" [:toParameterSyntax ${:name}]

    #puts stderr "Attribute.init valueParam for [::nx::core::current object] is $(mparam)"
    if {$(mparam) ne ""} {
      if {[info exists :multivalued] && ${:multivalued}} {
        #puts stderr "adding assign [list obj var value:$(mparam),multivalued] // for [::nx::core::current object] with $(mparam)"
        :method assign [list obj var value:$(mparam),multivalued,slot=[::nx::core::current object]] {
          ::nx::core::setvar $obj $var $value
        }
        #puts stderr "adding add method for [::nx::core::current object] with value:$(mparam)"
        :method add [list obj prop value:$(mparam),slot=[::nx::core::current object] {pos 0}] {
          ::nx::core::next
        }
      } else {
        #puts stderr "SV adding assign [list obj var value:$(mparam)] // for [::nx::core::current object] with $(mparam)"
        :method assign [list obj var value:$(mparam),slot=[::nx::core::current object]] {
          ::nx::core::setvar $obj $var $value
        }

      }
    }
    if {[info exists :valuechangedcmd]} {
      append __initcmd ":trace add variable [list ${:name}] write \
	\[list [::nx::core::current object] __value_changed_cmd \[::nx::core::current object\] [list [set :valuechangedcmd]]\]"
    }
    if {$__initcmd ne ""} {
      set :initcmd $__initcmd
    }
  }
  
  # mixin class for optimizing slots
  ::nx::Class create ::nx::Attribute::Optimizer {

    :method method args  {::nx::core::next; :optimize}
    :method forward args {::nx::core::next; :optimize}
    :protected method init args {::nx::core::next; :optimize}

    :public method optimize {} {
      #puts stderr OPTIMIZER-[info exists :incremental]
      if {![info exists :methodname]} {return}
      set object [expr {${:per-object} ? {object} : {}}]
      if {${:per-object}} {
        set perObject -per-object
        set infokind Object
      } else {
        set perObject ""
        set infokind Class
      }
      if {[::nx::core::cmd::${infokind}Info::method ${:domain} name ${:name}] ne ""} {
        #puts stderr "OPTIMIZER RESETTING ${:domain} slot ${:name}"
        ::nx::core::forward ${:domain} {*}$perObject ${:name} \
            ${:manager} \
            [list %1 [${:manager} defaultmethods]] %self \
            ${:methodname}
      }
      #puts stderr "OPTIMIZER incremental [info exists :incremental] def '[set :defaultmethods]'"
      if {[info exists :incremental] && ${:incremental}} return
      if {[set :defaultmethods] ne {get assign}} return
      set assignInfo [:info callable -which assign]
      #puts stderr "OPTIMIZER assign=$assignInfo//[lindex $assignInfo {end 0}]//[:info precedence]"

      if {$assignInfo ne "::nx::ObjectParameterSlot alias assign ::nx::core::setvar" &&
          [lindex $assignInfo {end 0}] ne "::nx::core::setvar" } return
      if {[:info callable -which get] ne "::nx::ObjectParameterSlot alias get ::nx::core::setvar"} return

      array set "" [:toParameterSyntax ${:name}]
      if {$(mparam) ne ""} {
        set setterParam [lindex $(oparam) 0]
        #puts stderr "setterParam=$setterParam, op=$(oparam)"
      } else {
        set setterParam ${:name}
      }
      ::nx::core::setter ${:domain} {*}$perObject $setterParam
      #puts stderr "::nx::core::setter ${:domain} {*}$perObject $setterParam"
    }
  }
  # register the optimizer per default
  ::nx::Attribute mixin add ::nx::Attribute::Optimizer

  ############################################
  # Define method "attribute" for convenience
  ############################################
  ::nx::Class method attribute {spec {-slotclass ::nx::Attribute} {initblock ""}} {
    $slotclass createFromParameterSyntax [::nx::core::current object] -initblock $initblock {*}$spec
  }
  ::nx::Object method attribute {spec {-slotclass ::nx::Attribute} {initblock ""}} {
    $slotclass createFromParameterSyntax [::nx::core::current object] -per-object -initblock $initblock {*}$spec
  }
  ############################################
  # Define method "parameter" for backward 
  # compatibility and convenience
  ############################################
  ::nx::Class public method parameter arglist {
  
    foreach arg $arglist {
      ::nx::Attribute createFromParameterSyntax [::nx::core::current object] {*}$arg
    }
    # todo needed?
    set slot [::nx::core::current object]::slot
    if {![::nx::core::objectproperty $slot object]} {::nx::Object create $slot}
    ::nx::core::setvar $slot __parameter $arglist
  }
  ::nx::core::method ::nx::classInfo parameter {class} {
    set slot ${class}::slot
    if {![::nx::core::objectproperty $slot object]} {::nx::Object create $slot}
    if {[::nx::core::existsvar $slot __parameter]} {
      return [::nx::core::setvar $slot __parameter]
    }
    return ""
  }

  ##################################################################
  # now the slots are defined; now we can defines the Objects or 
  # classes with parameters more easily than above.
  ##################################################################

  # remove helper proc
  proc createBootstrapAttributeSlots {} {}

  ##################################################################
  # create user-level converter/checker based on ::nx::core primitves
  ##################################################################

  ::nx::Slot method type=hasmixin {name value arg} {
    if {![::nx::core::objectproperty $value hasmixin $arg]} {
      error "expected object with mixin $arg but got \"$value\" for parameter $name"
    }
    return $value
  }

  ::nx::Slot method type=baseclass {name value} {
    if {![::nx::core::objectproperty $value baseclass]} {
      error "expected baseclass but got \"$value\" for parameter $name"
    }
    return $value
  }

  ::nx::Slot method type=metaclass {name value} {
    if {![::nx::core::objectproperty $value metaclass]} {
      error "expected metaclass but got \"$value\" for parameter $name"
    }
    return $value
  }

}

##################################################################
# Create a mixin class to overload method "new" such it does not
# allocate new objects in ::nx::*, but in the specified object
# (without syntactic overhead).
##################################################################

::nx::Class create ::nx::ScopedNew -superclass ::nx::Class {
  
  :attribute {withclass ::nx::Object}
  :attribute container

  :protected method init {} {
    :public method new {-childof args} {
      ::nx::core::importvar [::nx::core::current class] {container object} withclass
      if {![::nx::core::objectproperty $object object]} {
        $withclass create $object
      }
      eval ::nx::core::next -childof $object $args
    }
  }
}

##################################################################
# The method 'contains' changes the namespace in which objects with
# realtive names are created.  Therefore, 'contains' provides a
# friendly notation for creating nested object structures. Optionally,
# creating new objects in the specified scope can be turned off.
##################################################################

::nx::Object public method contains {
   {-withnew:boolean true}
   -object
   {-class ::nx::Object}
   cmds
 } {
  if {![info exists object]} {set object [::nx::core::current object]}
  if {![::nx::core::objectproperty $object object]} {$class create $object}
  $object requireNamespace
  if {$withnew} {
    set m [::nx::ScopedNew new -volatile \
	       -container $object -withclass $class]
    ::nx::Class mixin add $m end
    # TODO: the following is not pretty; however, contains might build xotcl1 and next objects.
    if {[::nx::core::objectproperty ::xotcl::Class class]} {::xotcl::Class instmixin add $m end}
    namespace eval $object $cmds
    ::nx::Class mixin delete $m
    if {[::nx::core::objectproperty ::xotcl::Class class]} {::xotcl::Class instmixin delete $m}
  } else {
    namespace eval $object $cmds
  }
}
::nx::Class forward slots %self contains \
    -object {%::nx::core::dispatch [::nx::core::current object] -objscope ::subst [::nx::core::current object]::slot}

##################################################################
# copy/move implementation
##################################################################

::nx::Class create ::nx::CopyHandler {

  :attribute {targetList ""}
  :attribute {dest ""}
  :attribute objLength

  :method makeTargetList {t} {
    lappend :targetList $t
    #puts stderr "COPY makeTargetList $t target= ${:targetList}"
    # if it is an object without namespace, it is a leaf
    if {[::nx::core::objectproperty $t object]} {
      if {[$t info hasnamespace]} {
        # make target list from all children
        set children [$t info children]
      } else {
        # ok, no namespace -> no more children
        return
      }
    }
    # now append all namespaces that are in the obj, but that
    # are not objects
    foreach c [namespace children $t] {
      if {![::nx::core::objectproperty $c object]} {
        lappend children [namespace children $t]
      }
    }
    
    # a namespace or an obj with namespace may have children
    # itself
    foreach c $children {
      :makeTargetList $c
    }
  }


  :method copyNSVarsAndCmds {orig dest} {
    ::nx::core::namespace_copyvars $orig $dest
    ::nx::core::namespace_copycmds $orig $dest
  }

  # construct destination obj name from old qualified ns name
  :method getDest origin {
    set tail [string range $origin [set :objLength] end]
    return ::[string trimleft [set :dest]$tail :]
  }
  
  :method copyTargets {} {
    #puts stderr "COPY will copy targetList = [set :targetList]"
    foreach origin [set :targetList] {
      set dest [:getDest $origin]
      if {[::nx::core::objectproperty $origin object]} {
        # copy class information
        if {[::nx::core::objectproperty $origin class]} {
          set cl [[$origin info class] create $dest -noinit]
          # class object
          set obj $cl
          $cl superclass [$origin info superclass]
          ::nx::core::assertion $cl class-invar [::nx::core::assertion $origin class-invar]
          ::nx::core::relation $cl class-filter [::nx::core::relation $origin class-filter]
          ::nx::core::relation $cl class-mixin [::nx::core::relation $origin class-mixin]
          :copyNSVarsAndCmds ::nx::core::classes$origin ::nx::core::classes$dest
        } else {
          # create obj
          set obj [[$origin info class] create $dest -noinit]
        }
        # copy object -> may be a class obj
        ::nx::core::assertion $obj check [::nx::core::assertion $origin check]
        ::nx::core::assertion $obj object-invar [::nx::core::assertion $origin object-invar]
        ::nx::core::relation $obj object-filter [::nx::core::relation $origin object-filter]
        ::nx::core::relation $obj object-mixin [::nx::core::relation $origin object-mixin]
        if {[$origin info hasnamespace]} {
          $obj requireNamespace
        }
      } else {
        namespace eval $dest {}
      }
      :copyNSVarsAndCmds $origin $dest
      foreach i [::nx::core::cmd::ObjectInfo::forward $origin] {
        eval [concat ::nx::core::forward $dest -per-object $i [::nx::core::cmd::ObjectInfo::forward $origin -definition $i]]
      }
      if {[::nx::core::objectproperty $origin class]} {
        foreach i [::nx::core::cmd::ClassInfo::forward $origin] {
          eval [concat ::nx::core::forward $dest $i [::nx::core::cmd::ClassInfo::forward $origin -definition $i]]
        }
      }
      set traces [list]
      foreach var [$origin info vars] {
        set cmds [::nx::core::dispatch $origin -objscope ::trace info variable $var]
        if {$cmds ne ""} {
          foreach cmd $cmds {
            foreach {op def} $cmd break
            #$origin trace remove variable $var $op $def
            if {[lindex $def 0] eq $origin} {
              set def [concat $dest [lrange $def 1 end]]
            }
            $dest trace add variable $var $op $def
          }
        }
      }
      #puts stderr "====="
    }
    # alter 'domain' and 'manager' in slot objects for classes
    foreach origin [set :targetList] {
      if {[::nx::core::objectproperty $origin class]} {
        set dest [:getDest $origin]
        foreach oldslot [$origin info slots] {
          set newslot [::nx::Slot slotName [namespace tail $oldslot] $dest] 
          if {[$oldslot domain] eq $origin}   {$newslot domain $cl}
          if {[$oldslot manager] eq $oldslot} {$newslot manager $newslot}
        }
      }
    }
  }
  
  :public method copy {obj dest} {
    #puts stderr "[::nx::core::current object] copy <$obj> <$dest>"
    set :objLength [string length $obj]
    set :dest $dest
    :makeTargetList $obj
    :copyTargets
  }
}


::nx::Object public method copy newName {
  if {[string compare [string trimleft $newName :] [string trimleft [::nx::core::current object] :]]} {
    [::nx::CopyHandler new -volatile] copy [::nx::core::current object] $newName
  }
}

::nx::Object public method move newName {
  if {[string trimleft $newName :] ne [string trimleft [::nx::core::current object] :]} {
    if {$newName ne ""} {
      :copy $newName
    }
    ### let all subclasses get the copied class as superclass
    if {[::nx::core::objectproperty [::nx::core::current object] class] && $newName ne ""} {
      foreach subclass [:info subclass] {
        set scl [$subclass info superclass]
        if {[set index [lsearch -exact $scl [::nx::core::current object]]] != -1} {
          set scl [lreplace $scl $index $index $newName]
          $subclass superclass $scl
        }
      }	
    }
    :destroy
  }
}

#######################################################
# some utilities
#######################################################

namespace eval ::nx {
  #
  # Provide an ensemble-like interface to the nx::core primitiva to
  # access variables. Note that aliasing in the next scripting
  # framework is faster than namespace-ensembles.
  #
  Object create ::nx::var {
    :alias exists ::nx::core::existsvar 
    :alias import ::nx::core::importvar
    :alias set ::nx::core::setvar
  }

  interp alias {} ::nx::self {} ::nx::core::current object
}


namespace eval ::nx::core {
  #
  # determine platform aware temp directory
  #
  proc tmpdir {} {
    foreach e [list TMPDIR TEMP TMP] {
      if {[info exists ::env($e)] \
              && [file isdirectory $::env($e)] \
              && [file writable $::env($e)]} {
        return $::env($e)
      }
    }
    if {$::tcl_platform(platform) eq "windows"} {
      foreach d [list "C:\\TEMP" "C:\\TMP" "\\TEMP" "\\TMP"] {
        if {[file isdirectory $d] && [file writable $d]} {
          return $d
        }
      }
    }
    return /tmp
  }  

  namespace export tmpdir 
}

#######################################################################
# common code for all xotcl versions
namespace eval ::nx {

  # export the contents for all xotcl versions
  namespace export Object Class next self current

  # TODO should not be necessary in the future
  namespace export Attribute

  # if HOME is not set, and ~ is resolved, Tcl chokes on that
  if {![info exists ::env(HOME)]} {set ::env(HOME) /root}

  set ::nx::confdir ~/.xotcl
  set ::nx::logdir $::nx::confdir/log
  
  unset bootstrap
}

#
# The following will go away
#
#namespace eval ::xotcl {
#  namespace import ::nx::core::use
#}

#foreach ns {::next ::nx::core} {
#  puts stderr "$ns exports [namespace eval $ns {lsort [namespace export]}]"
#}
