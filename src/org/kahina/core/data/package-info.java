/**
* The central package for Kahina's data model, subpackages provide support for many data types.
* <p>
* A Kahina instance will often need to administer a variety of objects that are associated with computation steps.
* All these data should be stored in instances of classes extending {@link org.kahina.core.data.KahinaObject}.
* At the moment, KahinaObject only enforces data to be serializable,
* but this will soon be extended by the need to implement for each data type
* a method for import and export in an XML format.
* <p>
* The {@link org.kahina.core.data.KahinaObject} type is highly relevant for many other parts of the system.
* By default, only objects of type KahinaObject
* can be displayed in views that are part of a custom user interface. 
* Furthermore, behaviors can only be defined on KahinaObjects,
* and only KahinaObjects can be stored using Kahina's central data storage mechanism. 
* <p>
* @since 1.0 
*/

package org.kahina.core.data;