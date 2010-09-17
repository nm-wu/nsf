package require nx; namespace import -force ::nx::*
::nx::configure defaultMethodProtection false
package require nx::test

Test parameter count 10
Test case alias-preliminaries { 
  
  # The system methods of Object are either alias or forwarders
  ? {lsort [::nx::ObjectParameterSlot info methods -methodtype alias]} {assign get}
  ? {::nx::ObjectParameterSlot info method definition get} "::nx::ObjectParameterSlot alias get ::nsf::setvar"

  # define an alias and retrieve its definition
  set cmd "::nx::Object alias -objscope set ::set"
  eval $cmd
  ? {Object info method definition set} $cmd
  
}

Test case alias-simple { 
  # define an alias and retrieve its definition
  Class create Base {
    :method foo {{-x 1}} {return $x}
  }

  Class create Foo
  ::nsf::alias ::Foo foo ::nsf::classes::Base::foo
  
  ? {Foo info method definition foo} "::Foo alias foo ::nsf::classes::Base::foo"
  
  Foo create f1
  ? {f1 foo} 1
  ? {f1 foo -x 2} 2
  ? {Foo info methods -methodtype alias} "foo"
  
  ? {Base info methods -methodtype scripted} {foo}
  ? {Foo info methods -methodtype scripted} {}
  ? {Foo info methods -methodtype alias} {foo}
  Base method foo {} {}
  ? {Foo info methods -methodtype alias} ""
  ? {Base info methods -methodtype scripted} {}
  ? {Foo info methods -methodtype scripted} {}
  ? {Foo info method definition foo} ""
  

  Base method foo {{-x 1}} {return $x}
  ::nsf::alias ::Foo foo ::nsf::classes::Base::foo
  
  ? {Base info methods -methodtype scripted} {foo} "defined again"
  ? {Foo info methods -methodtype alias} {foo} "aliased again"
  Foo method foo {} {}
  ? {Base info methods -methodtype scripted} {foo} "still defined"
  ? {Foo info methods -methodtype alias} {} "removed"
}

Test case alias-chaining {
  #
  # chaining aliases
  #
  
  Class create T
  Class create S
  T create t
  S create s
  
  
  T method foo args { return [current class]->[current method] }
  ::nsf::alias T FOO ::nsf::classes::T::foo 
  
  ? {t foo} ::T->foo
  ? {t FOO} ::T->foo
  
  ? {lsort [T info methods]} {FOO foo}
  T method foo {} {}
  ? {lsort [T info methods]} {} "alias is deleted"
  
  # puts stderr "double indirection"
  T method foo args { return [current class]->[current method] }
  ::nsf::alias T FOO ::nsf::classes::T::foo 
  ::nsf::alias S BAR ::nsf::classes::T::FOO
  
  ? {T info methods -methodtype alias} "FOO"
  ? {T info method definition FOO} "::T alias FOO ::nsf::classes::T::foo"
  ? {lsort [T info methods]} {FOO foo}
  ? {S info methods} {BAR}
  T method FOO {} {}
  ? {T info methods} {foo}
  ? {S info methods} {BAR}
  ? {s BAR} ::S->foo
  ? {t foo} ::T->foo
  ? {S info method definition BAR} "::S alias BAR ::nsf::classes::T::FOO"
  
  
  T method foo {} {}
  ? {T info methods} {}
  ? {S info methods} {}
  
  T method foo args { return [current class]->[current method] }
  ::nsf::alias T FOO ::nsf::classes::T::foo 
  ::nsf::alias S BAR ::nsf::classes::T::FOO
  
  ? {lsort [T info methods]} {FOO foo}
  ? {S info methods} {BAR}
  T method foo {} {}
  ? {S info methods} {}
  ? {T info methods} {}
  
  T method foo args { return [current class]->[current method] }
  T class-object method bar args { return [current class]->[current method] }
  ::nsf::alias T -per-object FOO ::nsf::classes::T::foo 
  ::nsf::alias T -per-object BAR ::T::FOO 
  ::nsf::alias T -per-object ZAP ::T::BAR 
  ? {T info methods} {foo}
  ? {lsort [T class-object info methods -methodtype alias]} {BAR FOO ZAP}
  ? {lsort [T class-object info methods]} {BAR FOO ZAP bar}
  ? {t foo} ::T->foo
  ? {T class-object info method definition ZAP} {::T class-object alias ZAP ::T::BAR}
  
  ? {T FOO} ->foo
  ? {T BAR} ->foo
  ? {T ZAP} ->foo
  ? {T bar} ->bar
  T class-object method FOO {} {}
  ? {T info methods} {foo}
  ? {lsort [T class-object info methods]} {BAR ZAP bar}
  ? {T BAR} ->foo
  ? {T ZAP} ->foo
  rename ::T::BAR ""
  ? {T info methods} {foo}
  ? {lsort [T class-object info methods]} {ZAP bar}
  #? {T BAR} ""; #  now calling the proc defined above, alias chain seems intact
  ? {T ZAP} ->foo; # is ok, still pointing to 'foo'
  #T class-object method BAR {} {}
  ? {T info methods} {foo}
  ? {lsort [T class-object info methods]} {ZAP bar}
  ? {T ZAP} ->foo
  T method foo {} {}
  ? {T info methods} {}
  ? {lsort [T class-object info methods]} {bar}
}

Test case alias-per-object {

  Class create T {
    :class-object method bar args { return [current class]->[current method] }
    :create t
  }
  proc ::foo args { return [current class]->[current method] }

  #
  # per-object methods as per-object aliases
  #
  T class-object method m1 args { return [current class]->[current method] }
  ::nsf::alias T -per-object M1 ::T::m1 
  ::nsf::alias T -per-object M11 ::T::M1 
  ? {lsort [T class-object info methods]} {M1 M11 bar m1}
  ? {T m1} ->m1
  ? {T M1} ->m1
  ? {T M11} ->m1
  T class-object method M1 {} {}
  ? {lsort [T class-object info methods]} {M11 bar m1}
  ? {T m1} ->m1
  ? {T M11} ->m1
  T class-object method m1 {} {}
  ? {lsort [T class-object info methods]} {bar}
  
  #
  # a proc as alias
  #
  
  proc foo args { return [current class]->[current method] }
  ::nsf::alias T FOO1 ::foo 
  ::nsf::alias T -per-object FOO2 ::foo
  #
  # ! per-object alias referenced as per-class alias !
  #
  ::nsf::alias T BAR ::T::FOO2
  ? {lsort [T class-object info methods]} {FOO2 bar}
  ? {lsort [T info methods]} {BAR FOO1}
  ? {T FOO2} ->foo
  ? {t FOO1} ::T->foo
  ? {t BAR} ::T->foo
  #
  # delete proc
  #
  rename ::foo ""
  ? {lsort [T class-object info methods]} {bar}
  ? {lsort [T info methods]} {}
}


# namespaced procs + namespace deletion
Test case alias-namespaced {
  Class create T {
    :class-object method bar args { return [current class]->[current method] }
    :create t
  }
  
  namespace eval ::ns1 {
    proc foo args { return [current class]->[current method] }
    proc bar args { return [uplevel 2 {set _}] }
    proc bar2 args { upvar 2 _ __; return $__}
  }
  
  ::nsf::alias T FOO ::ns1::foo
  ::nsf::alias T BAR ::ns1::bar
  ::nsf::alias T BAR2 ::ns1::bar2
  ? {lsort [T info methods]} {BAR BAR2 FOO}
  set ::_ GOTYA
  ? {t FOO} ::T->foo
  ? {t BAR} GOTYA
  ? {t BAR2} GOTYA
  namespace delete ::ns1
  ? {info procs ::ns1::*} {}
  ? {lsort [T info methods]} {}
  
  # per-object namespaces
  
  Class create U
  U create u
  ? {namespace exists ::U} 0
  U class-object method zap args { return [current class]->[current method] }
  ::nsf::alias ::U -per-object ZAP ::U::zap 
  U require namespace
  ? {namespace exists ::U} 1
  
  U class-object method bar args { return [current class]->[current method] }
  ::nsf::alias U -per-object BAR ::U::bar
  ? {lsort [U class-object info methods]} {BAR ZAP bar zap}
  ? {U BAR} ->bar
  ? {U ZAP} ->zap
  namespace delete ::U
  ? {namespace exists ::U} 0
  ? {lsort [U class-object info methods]} {}
  ? {U info lookup methods BAR} ""
  ? {U info lookup methods ZAP} ""
  
  ::U destroy
}

# dot-resolver/ dot-dispatcher used in aliased proc

Test case alias-dot-resolver {

  Class create V {
    set :z 1
    :method bar {z} { return $z }
    :class-object method bar {z} { return $z }
    :create v {
      set :z 2
    }
  }
  ? {lsort [V info vars]} {z}

  ? {lsort [V info vars]} {z}
  ? {lsort [v info vars]} {z}

  proc ::foo args { return [:bar ${:z}]-[set :z]-[:bar [set :z]] }

  ::nsf::alias V FOO1 ::foo 
  ::nsf::alias V -per-object FOO2 ::foo

  ? {lsort [V class-object info methods]} {FOO2 bar}
  ? {lsort [V info methods]} {FOO1 bar}

  ? {V FOO2} 1-1-1
  ? {v FOO1} 2-2-2
  V method FOO1 {} {}
  ? {lsort [V info methods]} {bar}
  rename ::foo ""
  ? {lsort [V class-object info methods]} {bar}
}

#
# Tests for the ::nsf::alias store, used for introspection for
# aliases. The alias store (an associative variable) is mostly
# necessary for for the direct aliases (e.g. aliases to C implemented
# tcl commands), for which we have no stubs at the place where the
# alias was registered.
#

#
# structure of the ::nsf::alias store:
# <object>,<alias_name>,<per_object> -> <aliased_cmd>
#

Object create o
Class create C

o method bar args {;}

? {info vars ::nsf::alias} ::nsf::alias
? {array exists ::nsf::alias} 1 

proc ::foo args {;}
::nsf::alias ::o FOO ::foo
::nsf::alias ::C FOO ::foo
? {info exists ::nsf::alias(::o,FOO,1)} 1
? {info exists ::nsf::alias(::C,FOO,0)} 1
? {array get ::nsf::alias ::o,FOO,1} "::o,FOO,1 ::foo"
? {array get ::nsf::alias ::C,FOO,0} "::C,FOO,0 ::foo"
? {o info method definition FOO} "::o alias FOO ::foo"
? {C info method definition FOO} "::C alias FOO ::foo"

::nsf::alias o FOO ::o::bar
? {info exists ::nsf::alias(::o,FOO,1)} 1
? {array get ::nsf::alias ::o,FOO,1} "::o,FOO,1 ::o::bar"
? {o info method definition FOO} "::o alias FOO ::o::bar"

# AliasDelete in XOTclRemoveObjectMethod
o method FOO {} {}
? {info exists ::nsf::alias(::o,FOO,1)} 0
? {array get ::nsf::alias ::o,FOO,1} ""
? {o info method definition FOO} ""

# AliasDelete in XOTclRemoveClassMethod
C method FOO {} {}
? {info exists ::nsf::alias(::C,FOO,0)} 0
? {array get ::nsf::alias ::C,FOO,0} ""
? {C info method definition FOO} ""

::nsf::alias ::o BAR ::foo
::nsf::alias ::C BAR ::foo  

# AliasDelete in XOTclAddObjectMethod
? {info exists ::nsf::alias(::o,BAR,1)} 1
::o method BAR {} {;}
? {info exists ::nsf::alias(::o,BAR,1)} 0

# AliasDelete in XOTclAddInstanceMethod
? {info exists ::nsf::alias(::C,BAR,0)} 1
::C method BAR {} {;}
? {info exists ::nsf::alias(::C,BAR,0)} 0

# AliasDelete in aliasCmdDeleteProc
::nsf::alias o FOO ::foo
? {info exists ::nsf::alias(::o,FOO,1)} 1
rename ::foo ""
? {info exists ::nsf::alias(::o,FOO,1)} 0

::nsf::alias o FOO ::o::bar
::nsf::alias o BAR ::o::FOO
? {info exists ::nsf::alias(::o,FOO,1)} 1
? {info exists ::nsf::alias(::o,BAR,1)} 1
o method bar {} {}
? {info exists ::nsf::alias(::o,FOO,1)} 0
? {info exists ::nsf::alias(::o,BAR,1)} 0

#
# pulling the rug out from the proc-alias deletion mechanism
#

proc ::foo args {;}
::nsf::alias C FOO ::foo
? {info exists ::nsf::alias(::C,FOO,0)} 1
unset ::nsf::alias(::C,FOO,0)
? {info exists ::nsf::alias(::C,FOO,0)} 0
? {C info method definition FOO} ""
? {C info methods -methodtype alias} FOO
rename ::foo ""
? {C info methods -methodtype alias} ""
? {info exists ::nsf::alias(::C,FOO,0)} 0
? {C info method definition FOO} ""

#
# test renaming of Tcl proc (actually sensed by the alias, though not
# reflected by the alias definition store)
# a) is this acceptable?
# b) sync ::nsf::alias upon "info method definition" calls? is this feasible,
# e.g. through rename traces?
#

C create c
proc ::foo args { return [current]->[current method]}
? {info exists ::nsf::alias(::C,FOO,0)} 0
::nsf::alias C FOO ::foo
? {info exists ::nsf::alias(::C,FOO,0)} 1
? {C info methods -methodtype alias} FOO
rename ::foo ::foo2
? {info exists ::nsf::alias(::C,FOO,0)} 1
? {C info methods -methodtype alias} FOO
? {c FOO} ::c->foo2
? {C info method definition FOO} "::C alias FOO ::foo"; # should be ::foo2 (!)


#
# Check resolving of namespace imported classes
# and when a class is aliased via "interp alias"
#
Test case class-resolve {
  namespace eval ::ns1 {
    nx::Class create A {:method foo {} {::nx::current class}}
    nx::Class create B {:method foo {} {::nx::current class}}
    namespace export A
  }
  
  namespace eval ::ns2 {
    # namespace import Class A from namespace ns1
    namespace import ::ns1::A
    ? {A create a1} ::ns2::a1
    ? {nx::Class create C -superclass A} ::ns2::C
    ? {C create c1} ::ns2::c1
    ? {c1 foo} ::ns1::A

    # "import" Class B from namespace ns1 via interp-alias
    interp alias {} ::ns2::B {} ::ns1::B
    ? {B create b1} ::ns2::b1
    ? {b1 foo} ::ns1::B
    ? {nx::Class create D -superclass B} ::ns2::D
    ? {D create d1} ::ns2::d1
    ? {d1 foo} ::ns1::B
  }
}

Test parameter count 10
Test case proc-alias {

  nx::Class create C {
    :method foo {} {upvar x y; info exists y}
    :method bar {} {set x 1; :foo}

    :alias bar_ [:info method handle bar]
    :alias foo_ [:info method handle foo]
    :method bar2 {} {set x 1; :foo_}

    :create c1
  }
  
  nx::Class create D {
    :method foo {} {:upvar x y; info exists y}
    :method bar {} {set x 1; :foo}

    :alias foo_ [:info method handle foo]
    :alias bar_ [:info method handle bar]
    :method bar2 {} {set x 1; :foo_}

    :create d1
  }

  nx::Class create M {
    :method foo args next
    :method bar args next
    :method foo_ args next
    :method bar_ args next
    :method bar_ args next
  }
  
  ? {c1 bar} 1
  ? {c1 bar_} 1
  ? {c1 bar2} 0 ;# upvar reaches into to alias-redirector

  ? {d1 bar} 1
  ? {d1 bar_} 1
  ? {d1 bar2} 1

  c1 mixin add M

  ? {c1 bar} 0   ;# upvar reaches into to mixin method
  ? {c1 bar_} 0  ;# upvar reaches into to mixin method
  ? {c1 bar2} 0  ;# upvar reaches into to mixin method

  d1 mixin add M

  ? {d1 bar} 1
  ? {d1 bar_} 1
  ? {d1 bar2} 1

}
