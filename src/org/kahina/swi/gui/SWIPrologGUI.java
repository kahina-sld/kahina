package org.kahina.swi.gui;

import java.io.BufferedInputStream;
import java.io.InputStream;

import org.kahina.core.gui.KahinaPerspective;
import org.kahina.core.io.util.XMLUtil;
import org.kahina.core.visual.tree.KahinaAbstractTreeView;
import org.kahina.core.visual.tree.KahinaLayeredTreeView;
import org.kahina.core.visual.tree.KahinaLayeredTreeViewPanel;
import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.sicstus.gui.SICStusPrologGUI;
import org.kahina.swi.SWIPrologDebuggerInstance;
import org.kahina.swi.SWIPrologStep;

public class SWIPrologGUI extends LogicProgrammingGUI
{
	public SWIPrologGUI(Class<? extends SWIPrologStep> stepType, SWIPrologDebuggerInstance kahina)
	{
		super(stepType, kahina);
	}
	
	@Override
	public KahinaAbstractTreeView generateTreeView()
	{
		return new KahinaLayeredTreeView(KahinaLayeredTreeViewPanel.Orientation.HORIZONTAL, kahina, 0);
	}
	
    public KahinaPerspective generateInitialPerspective()
    {
        //TODO: load last perspective instead of only default perspective from XML
        InputStream xmlStream = new BufferedInputStream(SWIPrologGUI.class.getResourceAsStream("kahinaswi-integrated.xml"));
        return KahinaPerspective.importXML(XMLUtil.parseXMLStream(xmlStream, false).getDocumentElement());
    }
}
