/**
* Provides a datatype for DAGs (directed acyclic graphs).
* <p>
* The abstract class {@link org.kahina.core.data.dag.KahinaDAG}
* defines the functionality that any DAG implementation is expected so support.
* This includes the possibility to extend the DAG structure, to define labels for both nodes and edges,
* abd to influence the appearance of nodes and eges via integer-encoded states.
* Unlike in the {@link org.kahina.core.data.graph.KahinaGraph} class,
* edges are directly modeled as objects with an ID space of their own.
* Any class implementing {@link org.kahina.core.data.dag.KahinaDAG} can be visualized
* via the {@link org.kahina.core.data.visual.dag} package.
* <p>
* {@link org.kahina.core.data.dag.KahinaMemDAG} is the reference DAG implementation which implements
* the functionality in a straightforward manner. It is not useful for more than medium-sized DAGs
* because the entire data is stored in memory (hence the name).
* <p>
* Instances of {@link org.kahina.core.data.dag.KahinaDAGEvent} are used internally to
* communicate changes to a DAG model. Every <code>KahinaDAGEvent</code> is of one of the types
* defined in {@link org.kahina.core.data.dag.KahinaDAGEventType}, with <code>NEW_NODE</code>
* being the only event type which is currently implemented.
* <p>
* @since 1.0 
*/

package org.kahina.core.data.dag;