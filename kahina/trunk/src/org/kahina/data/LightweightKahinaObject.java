package org.kahina.data;

/**
 * Clients who do not wish to implement their own database data store should
 * usually define their data types by extending this class. Subclasses of this
 * class must be implemented in such a way that the state of an object is fully
 * defined by its ID and the values of the <code>public</code> fields of types
 * <code>KahinaObject</code>, <code>KahinaObject[]</code>, <code>String</code>,
 * <code>String[]</code>, <code>Integer</code>, <code>Integer[]</code>,
 * <code>int</code>, and <code>int[]</code>. Only this information will be
 * stored and retrieved by default database stores like
 * {@link LightweightKahinaObjectDbDataStore}. <code>null</code> values of array
 * fields are treated exactly like empty arrays. In addition, subclasses are
 * required to provide a one-argument constructor that takes the ID of the
 * object to be constructed and passes it on (possibly via other constructors)
 * to the constructor of this class.
 * 
 * @author ke
 * 
 */
public class LightweightKahinaObject extends KahinaObject
{
	public LightweightKahinaObject(int id)
	{
		super(id);
	}
}
