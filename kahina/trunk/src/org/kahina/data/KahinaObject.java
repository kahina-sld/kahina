package org.kahina.data;

/**
 * "Kahina objects" are pieces of information that can be stored and retrieved
 * using a {@link DataManager}. They are instances of {@link KahinaObject}.
 * 
 * Clients can define their own data types by implementing this interface. The
 * runtime class of a Kahina object is referred to as its data type.
 * 
 * Kahina objects are uniquely identified by their data type and their ID (
 * {@link #id}, {@link #getID()}). Therefore, clients must ensure that each
 * Kahina object gets an ID that is unique within its data type. Usually, this
 * is done by having one static variable <code>nextID</code> per data type that
 * is incremented with each object creation.
 * 
 * @author ke
 * 
 */
public interface KahinaObject
{

	public int getID();
	
}
