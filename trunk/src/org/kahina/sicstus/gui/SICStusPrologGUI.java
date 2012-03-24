package org.kahina.sicstus.gui;

import java.io.BufferedInputStream;
import java.io.InputStream;

import org.kahina.core.gui.KahinaPerspective;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.io.util.XMLUtil;
import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.sicstus.SICStusPrologDebuggerInstance;
import org.kahina.sicstus.SICStusPrologStep;

public class SICStusPrologGUI extends LogicProgrammingGUI
{
	public SICStusPrologGUI(Class<? extends SICStusPrologStep> stepType, SICStusPrologDebuggerInstance instance)
	{
		super(stepType, instance);
	}
	
	@Override
	protected KahinaWindowManager createWindowManager()
	{
		return new SICStusPrologWindowManager((SICStusPrologDebuggerInstance) kahina);
	}

	@Override
	public void prepare()
	{
		try
		{
			displayMainViews();
			//TODO: load last perspective instead of only default perspective from XML
			InputStream xmlStream = new BufferedInputStream(SICStusPrologGUI.class.getResourceAsStream("kahinasicstus-integrated.xml"));
			windowManager.createWindows(KahinaPerspective.importXML(XMLUtil.parseXMLStream(xmlStream, false).getDocumentElement()));			
		}
		catch (NullPointerException e)
		{
			e.printStackTrace();
		}
	}
}
