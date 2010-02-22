package org.kahina.data;

/**
 * The base class of all Kahina objects, i.e. pieces of information that can be
 * stored and retrieved using a {@link DataManager}.
 * 
 * Clients can define their own data types by extending this class. The runtime
 * class of a Kahina object is referred to as its data type.
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
public class KahinaObject
{
	private int id;

	/**
	 * Creates a new Kahina object.
	 * 
	 * @param id
	 *            An ID that is unique within the data type of this Kahina
	 *            object.
	 */
	public KahinaObject(int id)
	{
		this.id = id;
	}

	public int getID()
	{
		return id;
	}
}
