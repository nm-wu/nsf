# What is the scheme of superclass linearisation used by NSF/NX (XOTcl 2)?
# ------------------------------------------------------------------------
#
#
# Linearisation schemes can be compared from two angles: On the one
# hand, there is the view of computing linearisations (i.e., the
# computational view). On the other hand, a linearisation can be
# evaluated against a set of desirable structural properties (i.e.,
# structure view).
#
# [[CompView]]
# On the computational view
# -------------------------
# 
# tbd. 

package req nx
package req nx::test
nx::Test parameter count 1

nx::Class create a
nx::Class create b
nx::Class create c
nx::Class create s -superclass {a b}
nx::Class create r -superclass {a c}
nx::Class create q -superclass {s r}

proc getDSCByDepthFirstPreorder {cls} {
  set dsc [list]
  foreach dsCls [$cls info superclass] {
    if {$dsCls eq "::nx::Object"} break;
    lappend dsc $dsCls {*}[getDSCByDepthFirstPreorder $dsCls]
  }
  return $dsc
}

? {getDSCByDepthFirstPreorder q} "::s ::a ::b ::r ::a ::c"

proc Loops {cls} {
  set dsc [getDSCByDepthFirstPreorder $cls]
  return [list [$cls] {*}[lreverse [dict keys [dict create {*}"[join [lreverse $dsc] " _ "] _"]]] ::nx::Object]
}

? {Loops q} "::q ::s ::b ::r ::a ::c ::nx::Object"
! {Loops q} [concat [q] [q info heritage]]

#
# [[StructView]]
# On the structure view
# ---------------------
#
# The following structural properties of linearisations have been
# proposed (see, e.g., <<Dylan>> for an overview):
# 
# - Acceptability: ...
# - observed Local Precdence Order (oLPO): ...
# - Monotonicity (a.k.a. in-order rule in <<MOP>>): ...
# - Compliance with the Extended Precedence Graph (EPG): ...
#
# Example 1 taken from <<Dylan>>:
#

nx::Class create GridLayout
nx::Class create HorizontalGrid -superclass GridLayout
nx::Class create VerticalGrid -superclass GridLayout

HorizontalGrid public method startingEdge {} {return 1;}
VerticalGrid public method startingEdge {} {return 2;}

nx::Class create HVGrid -superclass {HorizontalGrid VerticalGrid}

#
# Which method +startingEdge+ takes precedence for instances of the class +HVGrid+?
#
# In principle, both method implementations of +startingEdge+ are
# valid candidates for answering +startingEdge+ messages sent to
# instances of the class +HVGrid+ to an initial method
# implementation. In other words, both +HorizontalGrid+ or
# +VerticalGrid+ qualify as the most specific superclass of +HVGrid+
# (for the purpose of method lookup).
# 
# For deciding on the property of being the most specific superclass,
# NX adopts a basic notion of preserving the _local precedence order_. This
# order corresponds to the ordered set of direct superclasses (i.e.,
# +(HorizontalGrid,VerticalGrid)+) of a given class (i.e., +HVGrid+),
# as specified by the developer.
#
# From the perspective of the class +HVGrid+, the exact order of its direct
# superclasses +HorizontalGrid+ and +VerticalGrid+ is preserved in its overall
# class inheritance path: 
#

? {HVGrid info heritage} "::HorizontalGrid ::VerticalGrid ::GridLayout ::nx::Object"

# 
# HorizontalGrid is so coined the most specific superclass; and its
# implementation +startingEdge+ will answer first:
#

? {HVGrid create hv} "::hv"
? {::hv info lookup method startingEdge} "::nsf::classes::HorizontalGrid::startingEdge"

#
# From this, it follows that one can influence the choice of the most
# specific superclass by reversing the local precedence order; that
# is, by flipping the elements of the direct superclass pair:
# +(HorizontalGrid,VerticalGrid)+ becomes
# +(VerticalGrid,HorizontalGrid)+.
#

nx::Class create VHGrid -superclass {VerticalGrid HorizontalGrid}
? {VHGrid info heritage} "::VerticalGrid ::HorizontalGrid ::GridLayout ::nx::Object"
? {VHGrid create vh} "::vh"
? {::vh info lookup method startingEdge} "::nsf::classes::VerticalGrid::startingEdge"

#
# In the above inheritance structure of grid classes, the condition of
# local precedence is consistently preserved for each of the two most
# specific classes: +HVGrid+ and +VHGrid+. 
#
# However, can the condition of local precedence order (for a given class) hold for
# several levels of multiple inheritance? 
#
# To be precise, is the (local) order of direct superclasses for each
# class in an inheritance structure maintained in all class precedence
# lists of the class' subclasses? Not necessarily. Consider a class
# which declares both, +HVGrid+ and +VHGrid+, its direct superclasses:
#

nx::Class create ConfusedGrid -superclass {HVGrid VHGrid}
? {ConfusedGrid info heritage} "::HVGrid ::VHGrid ::VerticalGrid ::HorizontalGrid ::GridLayout ::nx::Object"
? {ConfusedGrid create cg} "::cg"
? {::cg info lookup method startingEdge} "::nsf::classes::VerticalGrid::startingEdge"

#
# Looking at the class predence list of +ConfusedGrid+, one learns
# that the pairing +(VerticalGrid,HorizontalGrid)+ violates the local
# precedence condition for +HVGrid+ which is
# +(HorizontalGrid,VerticalGrid)+. As a consequence, the +startEdge+
# implementation of +VerticalEdge+ will respond even though the
# developer declaring +HVGrid+ expected +startingEdge+ of +HVGrid+ to
# resolve first. In literature, this is considered controversial for
# two reasons:
#
# 1) Method combination: Reusing and incrementally refining the
# implementation of +startEdge+ by stepping up the class inheritance
# structure requires a stable chaining of method
# implementations. Without the local precedence condition observed for
# a class at one level of the inheritance structure being maintained
# throughout all lower levels (i.e., for its subclasses), the silent
# contract of reuse between method implementors can be broken.
#
# 2) Program comprehension: Not preserving the local precedence
# condition harms the developer's intuition when inspecting the
# (alien) class structure. For example, when inferring the order of
# method lookup in the overall inheritance structure from inspecting
# the direct superclass lists of the classes of her
# interest. Similarly, integrators (i.e., a developer providing a
# final class to a class library or abstract class framework) having
# the intention to reuse specific method implementations at their (the
# lowest) class level can be surprised. In the above example, the
# implementor could have shown her intention to reuse +startingEdge+
# of +HorizontalGrid+ by stating +HVGrid+ first in the direct
# superclass list of +ConfusedGrid+.
#
# 3) Ahead-of-time (AOT) method binding: Without the guarantees of
# preserving local precedence, certain optimisations (e.g.,
# dispatching) in a method dispatch infrastructure (for a language
# runtime with dynamic method dispatch) are rendered impossible or
# less effective. See the <<oLPOvsCaching, Appendix>> for an example
# of method caching in NX.
#
# A linearisation (such as the one of NX) which does not consider the
# local precedence condition at all levels of the inheritance structure
# produces _inconsistent_ class heterarchies (w.r.t. method combination
# and program comprehension).
#
# [bibliography]
# .Bibliography
# - [[[Dylan]]] Barrett, K., Cassels, B., Haahr, P., Moon, D. A.,
# Playford, K., & Withington, P. T. (1996). A Monotonic Superclass
# Linearization for Dylan. In Proceedings of the 1996 ACM SIGPLAN
# Conference on Object-Oriented Programming Systems, Languages &
# Applications (OOPSLA '96) (pp. 69--82). ACM.
# - [[[MOP]]] Kiczales, G., Rivieres, J., & Bobrow, D. (1991). The Art
# of the Metaobject Protocol.  MIT Press.
#
# [[appendix]]
# Appendix
# --------
#
# [[MOPEx1]]
# Heterarchy example taken from <<MOP>>, Section 3.4.1, p. 81
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#

nx::Class create a
nx::Class create b
nx::Class create c
nx::Class create s -superclass {a b}
nx::Class create r -superclass {a c}
nx::Class create q -superclass {s r}

#
# The current NSF/NX linearisation scheme corresponds to the Loops
# linearisation: non-monotonic, violates oLPO condition, acceptable. 
#
? {concat [q] [q info heritage]} "::q ::s ::b ::r ::a ::c ::nx::Object"

#
# In a CLOS-like scheme, the order would be (q,s,r,a,c,b,Object)
# (i.e., monotonic, but oLPO-violating), in Flavors
# (q,s,a,b,r,c,Object) (i.e., non-monotonic and oLPO-violating?) 
#
# [[oLPOvsCaching]]
# oLPO-violating NX linearisation vs. method caching in Tcl_Objs
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# The current NSF/NX scheme for caching method lookup results (the
# method's command structure) piggybacks the internal value
# representations (Tcl_Obj's intrep) of the first argument
# (+startingEdge+) to Tcl commands representing objects (+$grid+,
# below). As NSF/NX currently violates the oLPO condition (and for
# other reasons?), the caching of per-class methods must preserve the
# class (i.e., the context class) for which a method command was
# cached for the first time in a given Tcl_Obj (+startingEdge+
# below). This context class is then verified for identity with the
# current class upon each cache check. The following cache guard
# is evaluated: +current class == context class+.

proc render {grid} {
  set res [$grid startingEdge]
  # puts stderr [::nsf::__db_show_obj startingEdge]
  return $res
}

#
# Upon the first execution of +foo+, the method cache (underneath the
# Tcl_Obj representing +startingEdge+) becomes hot and, for subsequent
# calls to +foo+, shortcuts to
# +::nsf::classes::HorizontalGrid::startingEdge+ (if certain cache
# guards hold). The cache's context class says +HVGrid+.
#
? {render [HVGrid new]} 1

#
# Upon the second execution, there will be a cache hit (note, even if
# an instance different than the first instance of +HVGrid+ is passed
# to +foo+). The cache is valid because the context class (+HVGrid+ in
# the first run) is identical to the current class (again, +HVGrid+).
#
? {render [HVGrid new]} 1

#
# However, when passing an instance of a subclass of +HVGrid+ (i.e.,
# the inconsistency-causing +ConfusedGrid+ below), the above cache
# guard will fail and the cache will be invalidated. The following
# execution of +foo+ will clear the method cache and resolve
# +startingEdge+ from scratch.
#
? {render [ConfusedGrid new]} 2

#
# As a consequence, the message +startingEdge+ will "correctly"
# resolve to +::nsf::classes::VerticalGrid::startingEdge+. If the
# guard +context class == current class+ were not in place, the cached
# implementation of +startingEdge+ would be used; returning the
# "wrong" result of +1+. Put positively, if the oLPO condition were
# preserved, the above guard would not be necessary and, hence, the
# cache could be reused for more dispatch scenarios (for all context
# classes in an inheritance path respecting the oLPO condition).
#




