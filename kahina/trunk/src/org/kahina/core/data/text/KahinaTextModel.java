package org.kahina.core.data.text;

import org.kahina.core.data.KahinaObject;

public class KahinaTextModel extends KahinaObject
{
	/**
	 * 
	 */
	private static final long serialVersionUID = 210634361438935557L;
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
