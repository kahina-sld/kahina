/** 
* Provides classes for specifying the modification logic of complex data structures in a modular way.
* <p> 
* Some frequently used data structures such as trees and graphs often
* show complex behavior when step information arrives at a Kahina instance.
* This is especially the case when global tree or graph views are used to represent 
* complex structures made up of individual steps, such as decision trees and call structure visualizations.
* <p>
* The root class for all behaviors is {@link org.kahina.core.behavior.KahinaBehavior}.
* Usually, behaviors operate by listening on a controller for special events,
* and are defined to modify the structures they govern in some complex way.
* The behavior pattern is useful for separating the interface logic of adapted view components
* from the logic which determines the modifications to the data structures visualized.
* <p>
* {@link org.kahina.core.behavior.KahinaDAGBehavior} and {@link org.kahina.core.behavior.KahinaTreeBehavior}
* are stub behaviors for directed acylic graphs (i.e. {@link org.kahina.core.data.dag.KahinaTree}) 
* and trees (i.e. {@link org.kahina.core.data.tree.KahinaTree}), respectively.
* 
* @since 1.0 
*/ 

package org.kahina.core.behavior;