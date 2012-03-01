/**
* Contains the data model of the Kahina breakpoint system.
* <p>
* The breakpoint system is based on patterns which can be defined on step data,
* and possibly configurations of steps which are arranged in a tree.
* The event system distributes the news when such patterns match,
* allowing other components to react accordingly.
* Breakpoints can be associated with control actions such as stopping
* the monitored process, making a particular decision at a decision point,
* or issuing warnings.
* <p>
* Breakpoints are represented by {@link org.kahina.core.data.breakpoint.KahinaBreakpoint} objects,
* and can be generated using convenience methods in {@link org.kahina.core.data.breakpoint.KahinaBreakpointFactory}.
* Despite their name, breakpoints are used for much more comprehensive control automation
* than their counterparts in other debugging systems. 
* In this context, the type of a breakpoint, determining its semantics, 
* is defined by giving it one of the constant values in {@link org.kahina.core.data.breakpoint.KahinaBreakpointType}
* (some of these types are specific to logic programming, MOVE THEM!).
* <p>
* In a Kahina project, breakpoints will normally be grouped into
* a {@link org.kahina.core.data.breakpoint.KahinaBreakpointProfile},
* which is used to administer the different types of breakpoints in one place.
* If the content of a breakpoint profile is dumped into an XML representation
* e.g. as part of the project file, the entire state of the breakpoint
* system is included in this representation.
* <p>
* @since 1.0 
*/

package org.kahina.core.data.breakpoint;