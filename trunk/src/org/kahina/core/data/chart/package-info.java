/**
* Provides a datatype for charts (collections of labeled edges ranging over predefined segments).
* <p>
* The abstract class {@link org.kahina.core.data.chart.KahinaChart}
* defines the functionality that any chart implementation is expected so support.
* Most prominently, this includes the possibility to add edges, to modify the appearance of edges and segments,
* and to define a dependency hierarchy on the chart edges, e.g. for highlighting the components of edges
* which represent a combination of smaller edges.
* Any class implementing {@link org.kahina.core.data.chart.KahinaChart} can be visualized
* via the {@link org.kahina.core.data.visual.chart} package.
* <p>
* {@link org.kahina.core.data.chart.KahinaMemChart} is the reference chart implementation which implements
* the functionality in a straightforward manner. It is not useful for more than medium-sized charts
* because the entire data is stored in memory (hence the name).
* <p>
* @since 1.0 
*/

package org.kahina.core.data.chart;