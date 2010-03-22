package org.kahina.data;

public class KahinaDbString extends LightweightKahinaObject implements KahinaString
{
	
	private static int nextID = 0;
	
	public String value;

	public KahinaDbString(int id)
	{
		super(nextID++);
	}
	
	public String getValue() {
		return value;
	}

	@Override
	public void setValue(String value)
	{
		this.value = value;
	}

}
