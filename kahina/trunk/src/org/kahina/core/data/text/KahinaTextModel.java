package org.kahina.core.data.text;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.lightweight.LightweightKahinaObject;

public class KahinaTextModel extends KahinaObject implements LightweightKahinaObject
{
	public KahinaText text;
	
	public KahinaTextModel()
	{
		text = new KahinaText();
	}
	
	public KahinaTextModel(String absolutePathName)
	{
		text = new KahinaText(absolutePathName);
	}

}
