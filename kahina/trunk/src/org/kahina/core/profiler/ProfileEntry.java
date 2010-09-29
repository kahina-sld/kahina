package org.kahina.core.profiler;

import java.io.Serializable;

import org.kahina.core.util.Utilities;

public class ProfileEntry implements Serializable
{
	
	private static final long serialVersionUID = -8785694535266108407L;

	private final String name;
	
	private final String category;
	
	private final int hashCode;
	
	public ProfileEntry(String name, String category)
	{
		this.name = name;
		this.category = category;
		hashCode = Utilities.hashCode(name, category);
	}
	
	public String getName()
	{
		return name;
	}
	
	public String getCategory()
	{
		return category;
	}
	
	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof ProfileEntry))
		{
			return false;
		}
		ProfileEntry that = (ProfileEntry) o;
		return Utilities.equal(name, that.name) && Utilities.equal(category, that.category);
	}
	
	@Override
	public int hashCode()
	{
		return hashCode;
	}

}
