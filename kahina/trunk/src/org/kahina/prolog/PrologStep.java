package org.kahina.prolog;

import java.util.HashMap;
import java.util.Map;

import org.kahina.lp.LogicProgrammingStep;

public class PrologStep extends LogicProgrammingStep
{

	private static final long serialVersionUID = -119683692028732745L;

	private Map<String, String[]> keysByDirection = null;

	private Map<String, String[]> valuesByDirection = null;
	
	public PrologStep()
	{
	}
	
	public PrologStep(PrologStep original)
	{
		super(original);
		keysByDirection = new HashMap<String, String[]>(original.keysByDirection);
		valuesByDirection = new HashMap<String, String[]>(original.valuesByDirection);
	}

	public void setKeys(String direction, String[] keys)
	{
		if (keysByDirection == null)
		{
			keysByDirection = new HashMap<String, String[]>();
		}

		keysByDirection.put(direction, keys);
	}

	public void setValues(String direction, String[] values)
	{
		if (valuesByDirection == null)
		{
			valuesByDirection = new HashMap<String, String[]>();
		}

		valuesByDirection.put(direction, values);
	}
	
	@Override
	public PrologStep copy()
	{
		return new PrologStep(this);
	}

}
 