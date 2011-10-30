package org.kahina.tralesld.visual.workbench;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.swing.JComponent;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.event.KahinaEventTypes;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.visual.KahinaView;
import org.kahina.tralesld.TraleSLDState;
import org.kahina.tralesld.data.FeatureWorkbench;

public class FeatureWorkbenchView extends KahinaView<FeatureWorkbench>
{
	private List<String> nameList;
	
	public FeatureWorkbenchView(KahinaController control)
	{
		super(control);
	}
	
	public List<String> getNameList()
	{
		return nameList;
	}
	
	public void doDisplay()
	{
		recalculate();
	}
	
	public void recalculate()
	{
		nameList = new ArrayList<String>();
		nameList.addAll(model.getNames());
		Collections.sort(nameList);
	}

	@Override
	public JComponent makePanel(KahinaGUI gui)
	{
		//TODO: make state handling conceptually cleaner
		TraleSLDState state = (TraleSLDState) gui.getKahinaInstance().getState();
		FeatureWorkbenchViewPanel panel = new FeatureWorkbenchViewPanel(state);
        control.registerListener("redraw", panel);
		panel.setView(this);
		return panel;
	}

	public String getSignatureFileName() 
	{
		if (model == null)
		{
			return "none (no workbench loaded)";		
		}
		String signatureFile = model.getSignatureFileName();
		if (signatureFile == null)
		{
			return "none (signature not specified or not yet loaded)";
		}
		return signatureFile;
	}
	
	public String getTheoryFileName() 
	{
		if (model == null)
		{
			return "none (no workbench loaded)";		
		}
		String signatureFile = model.getSignatureFileName();
		if (signatureFile == null)
		{
			return "none (theory not specified or not yet loaded)";
		}
		return signatureFile;
	}

}