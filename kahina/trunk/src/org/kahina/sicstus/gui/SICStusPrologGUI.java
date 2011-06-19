package org.kahina.sicstus.gui;

import java.io.File;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.io.util.XMLUtilities;
import org.kahina.core.visual.tree.KahinaAbstractTreeView;
import org.kahina.core.visual.tree.KahinaLayeredTreeView;
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
	protected KahinaWindowManager createWindowManager(KahinaGUI gui, KahinaController control)
	{
		return new SICStusPrologWindowManager(gui, control);
	}

	@Override
	public void prepare(KahinaController control)
	{
		try
		{
			displayMainViews();
			//TODO: load last perspective instead of only default perspective from XML
			File xmlFile = new File(SICStusPrologGUI.class.getResource("kahinasicstus-integrated.xml").getFile());
			windowManager.createWindows(KahinaPerspective.importXML(XMLUtilities.parseXMLFile(xmlFile, false).getDocumentElement()));			
		}
		catch (NullPointerException e)
		{
			e.printStackTrace();
		}
	}
	
	/*@Override
	public KahinaAbstractTreeView generateTreeView(KahinaController control)
	{
		return new KahinaLayeredTreeView(control, 0);
	}*/
}
