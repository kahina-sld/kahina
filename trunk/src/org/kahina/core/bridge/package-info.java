/** 
* Provides functionality for handling the interaction of a program with a Kahina-based debugger.
* <p> 
* The most common application of the Kahina architecture is the implementation
* of specialized debuggers for programs written in Java or other languages.
* The recommended practice for organizing the communication with a
* Kahina-based debugger is to channel the entire communication through a bridge, 
* which we define as a class that provides a lean interface
* with methods receiving the step details of the monitored computation.
* A bridge will commonly implement some interaction logic,
* store step details in a step database, and generate events to be processed by behaviors.
* <p>
* A custom bridge should inherit from {@link org.kahina.core.bridge.KahinaBridge},
* which already predefines some event handling code, and registers itself
* as a listener for control events.
* The event type {@link org.kahina.core.bridge.KahinaStepDescriptionEvent}
* can be used to store description strings for steps,
* thereby providing most basic functionality for storing step data.
* <p>
* The event type {@link org.kahina.core.bridge.KahinaBridgePauseEvent}
* is important for the rudimentary breakpoint system implemented in the
* core Kahina package, ensuring that the computations
* between breakpoint matches can be paused if they take too long.
* 
* @since 1.0 
*/ 

package org.kahina.core.bridge;