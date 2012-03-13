/** 
* Provides the essential core classes for the Kahina architecture.
* <p> 
* The root package includes the class {@link org.kahina.core.KahinaInstance}
* which is the central piece of the architecture because it groups together
* the step database, the controller, the bridge, and configuration information.
* It also administers two instances of {@link org.kahina.core.control.KahinaController},
* and allows other components to dispatch events through these controllers.
* <p>
* The configuration data are grouped together in a {@link org.kahina.core.KahinaState},
* whereas the step information is stored in a collection of {@link org.kahina.core.KahinaStep} objects.
* <p>
* {@link org.kahina.core.KahinaException} is Kahina's basic exception datatype,
* from which all specialized exception types in various parts of the system should inherit.
* 
* @since 1.0 
*/ 

package org.kahina.core;