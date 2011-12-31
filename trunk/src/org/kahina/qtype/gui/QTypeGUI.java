package org.kahina.qtype.gui;

import java.io.BufferedInputStream;
import java.io.InputStream;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.io.util.XMLUtilities;
import org.kahina.core.visual.tree.KahinaListTreeView;
import org.kahina.qtype.QTypeDebuggerInstance;
import org.kahina.qtype.QTypeStep;
import org.kahina.qtype.data.tree.QTypeLayerDecider;
import org.kahina.sicstus.gui.SICStusPrologGUI;
import org.kahina.tralesld.gui.TraleSLDWindowManager;

public class QTypeGUI extends SICStusPrologGUI
{

	public QTypeGUI(Class<? extends QTypeStep> stepType, QTypeDebuggerInstance instance, KahinaController control)
	{
		super(stepType, instance, control);
	}

	@Override
	public void prepare(KahinaController control)
	{
		try
		{
			displayMainViews();
			//TODO: load last perspective instead of only default perspective from XML
			InputStream xmlStream = new BufferedInputStream(QTypeGUI.class.getResourceAsStream("kahinaqtype-integrated.xml"));
			windowManager.createWindows(KahinaPerspective.importXML(XMLUtilities.parseXMLStream(xmlStream, false).getDocumentElement()));	
		}
		catch (NullPointerException e)
		{
			e.printStackTrace();
		}
	}

	@Override
	protected KahinaListTreeView generateTreeView(KahinaController control)
	{
		return new KahinaListTreeView(control, 0, 1);
	}

	@Override
	public void displayMainViews()
	{
		super.displayMainViews();
		mainTreeView.getModel().setLayerDecider(new QTypeLayerDecider());
		mainTreeView.getSecondaryModel().setLayerDecider(new QTypeLayerDecider());
	}

	@Override
	protected KahinaWindowManager createWindowManager(KahinaGUI gui, KahinaController control)
	{
		return new QTypeWindowManager(gui, control);
	}

}
