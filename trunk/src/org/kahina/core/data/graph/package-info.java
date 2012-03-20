/**
* Provides a datatype for general graphs (directed and undirected).
* <p>
* The abstract class {@link org.kahina.core.data.graph.KahinaGraph}
* defines the functionality that any graph implementation is expected so support.
* This includes the possibility to extend the graph structure by additional vertices and (directed or undirected) edges, 
* to define labels for both vertices and edges, and
* to influence the appearance of nodes and eges via integer-encoded states.
* A <code>KahinaGraph</code> can also be imported from a file in TGF (Trivial Graph Format).
* Unlike in the {@link org.kahina.core.data.dag.KahinaDAG} class,
* edges are modeled indirectly without possessing an ID space of their own.
* Any class implementing {@link org.kahina.core.data.graph.KahinaGraph} can be visualized
* via the {@link org.kahina.core.data.visual.graph} package.
* <p>
* {@link org.kahina.core.data.graph.AdjacListsGraph} is the reference graph implementation
* based on an adjacency list representation of the graph structure. This implementation is
* especially useful for sparse graphs because no adjacency matrix is kept.
* <p>
* @since 1.0 
*/

package org.kahina.core.data.graph;