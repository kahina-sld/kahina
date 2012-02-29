/** 
* Provides the core classes for the Kahina architecture,
* including view components for many common datatypes such as trees and graphs.
* <p> 
* The root package includes the class {@link org.kahina.core.KahinaInstance}
* which is the central piece of the architecture because it groups together
* the step database, the controller, the bridge, and configuration information.
* <p>
* The configuration data are grouped together in a {@link org.kahina.core.KahinaState},
* whereas the step information is stored in a collection of {@link org.kahina.core.KahinaStep} objects.
* <p>
* {@link org.kahina.core.KahinaRunner} gives static access to Kahina's event system
* and the step database without a need to hand around a KahinaInstance object.
* This part of the architecture is not yet stable,
* and especially the functionality provided by {@link org.kahina.core.KahinaRunner}
* is planned to be treated in a more systematic manner.
* {@link org.kahina.core.KahinaException} is Kahina's basic exception datatype,
* from which all specialized exception types in various parts of the system should inherit.
* 
* @since 1.0 
*/ 

package org.kahina.core;