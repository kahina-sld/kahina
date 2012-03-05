package org.kahina.sicstus.gui;

import java.io.BufferedInputStream;
import java.io.InputStream;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.io.util.XMLUtil;
import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.sicstus.SICStusPrologDebuggerInstance;
import org.kahina.sicstus.SICStusPrologStep;

public class SICStusPrologGUI extends LogicProgrammingGUI
{
	public SICStusPrologGUI(Class<? extends SICStusPrologStep> stepType, SICStusPrologDebuggerInstance instance, KahinaController control)
	{
		super(stepType, instance, control);
	}
	
	@Override
	protected KahinaWindowManager createWindowManager()
	{
		return new SICStusPrologWindowManager((SICStusPrologDebuggerInstance) kahina);
	}

	@Override
	public void prepare(KahinaController control)
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
