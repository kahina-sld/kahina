package org.kahina.swi.gui;

import java.io.BufferedInputStream;
import java.io.InputStream;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.core.io.util.XMLUtilities;
import org.kahina.core.visual.tree.KahinaAbstractTreeView;
import org.kahina.core.visual.tree.KahinaLayeredTreeView;
import org.kahina.core.visual.tree.KahinaLayeredTreeViewPanel;
import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.swi.SWIPrologDebuggerInstance;
import org.kahina.swi.SWIPrologStep;

public class SWIPrologGUI extends LogicProgrammingGUI
{

	public SWIPrologGUI(Class<? extends SWIPrologStep> stepType, SWIPrologDebuggerInstance instance, KahinaController control)
	{
		super(stepType, instance, control);
	}
	
	@Override
	public KahinaAbstractTreeView generateTreeView(KahinaController control)
	{
		return new KahinaLayeredTreeView(KahinaLayeredTreeViewPanel.Orientation.HORIZONTAL, control, 0);
	}

	@Override
	public void prepare(KahinaController control)
	{
		try
		{
			displayMainViews();
			// TODO: load last perspective instead of only default perspective from XML
			InputStream xmlStream = new BufferedInputStream(SWIPrologGUI.class.getResourceAsStream("kahinaswi-integrated.xml"));
			windowManager.createWindows(KahinaPerspective.importXML(XMLUtilities.parseXMLStream(xmlStream, false).getDocumentElement()));			
		}
		catch (NullPointerException e)
		{
			e.printStackTrace();
		}
	}
}
