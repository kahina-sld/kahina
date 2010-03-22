package org.kahina.data;

public class KahinaMemString implements KahinaString
{
	
	private String value;

	@Override
	public String getValue()
	{
		return value;
	}
	
	@Override
	public void setValue(String value)
	{
		this.value = value;
	}

}
