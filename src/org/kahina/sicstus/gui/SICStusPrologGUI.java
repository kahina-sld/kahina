package org.kahina.sicstus.gui;

import java.io.BufferedInputStream;
import java.io.InputStream;

import org.kahina.core.gui.KahinaPerspective;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.io.util.XMLUtil;
import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.qtype.gui.QTypeGUI;
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
	
    public KahinaPerspective generateInitialPerspective()
    {
        //TODO: load last perspective instead of only default perspective from XML
        InputStream xmlStream = new BufferedInputStream(SICStusPrologGUI.class.getResourceAsStream("kahinasicstus-integrated.xml"));
        return KahinaPerspective.importXML(XMLUtil.parseXMLStream(xmlStream, false).getDocumentElement());
    }
}
