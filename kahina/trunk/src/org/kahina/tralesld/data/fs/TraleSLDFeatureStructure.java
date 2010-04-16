package org.kahina.tralesld.data.fs;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.lightweight.LightweightKahinaObject;

public class TraleSLDFeatureStructure extends KahinaObject implements LightweightKahinaObject
{
	public String grisuMessage;
	
	public TraleSLDFeatureStructure()
	{
		// do nothing
	}
	
	public TraleSLDFeatureStructure(String grisuMessage)
	{
		this.grisuMessage = grisuMessage;
	}
}
