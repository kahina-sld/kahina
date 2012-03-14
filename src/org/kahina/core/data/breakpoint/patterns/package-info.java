/** 
* Contains classes that implement patterns on step information for the breakpoint system.
* <p>
* Kahina's breakpoint system is built around the definition of patterns on step content,
* which can be combined using boolean connectives,
* and then used as elementary patterns to define patterns in a step tree.
* <p>
* The {@link org.kahina.core.data.breakpoint.patterns.TreeNodePattern} class
* is used to encode (boolean combinations of) node patterns.
* At the moment, string comparisons (including regular expressions) are allowed
* for matching step descriptions and edge labels in a step tree,
* and integer relations allow to match a step's ID and status values.
* TreeNodePatterns can be grouped recursively to boolean combinations
* using the negation, conjunction, disjunction, and implication connectives.
* <p>
* Tree patterns are represented directly by instances of {@link org.kahina.core.data.breakpoint.patterns.TreePattern},
* a tree structure with nodes of type {@link org.kahina.core.data.breakpoint.patterns.TreePatternNode},
* where each node is associated with an TreeNodePattern.
* <p>
* In order to make tree pattern matches efficiently computable,
* tree patterns are compiled into tree automata.
* A {@link org.kahina.core.data.breakpoint.patterns.TreeAutomaton} keeps track of the changes
* in a {@link org.kahina.core.data.tree.KahinaTree} object, firing a {@link org.kahina.core.data.breakpoint.patterns.KahinaTreeMatchEvent}
* in case the tree pattern it encodes is matched by.
* <p>
* Internally a TreeAutomaton is defined a collection of rules, and maintains annotations of the nodes in a KahinaTree.
* The rules are represented by instances of {@link org.kahina.core.data.breakpoint.patterns.TreeAutomatonRule},
* each of which combines a TreeNodePattern with a set of required annotations for the matched node's children.
* During breakpoint compilation an editing, {@link org.kahina.core.data.breakpoint.patterns.PatternFormatException}s
* are used to inform the interface components about specification errors for user feedback.
* <p>
* @since 1.0 
*/ 

package org.kahina.core.data.breakpoint.patterns;