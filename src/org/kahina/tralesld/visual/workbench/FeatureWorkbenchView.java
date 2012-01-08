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
import org.kahina.tralesld.bridge.AuxiliaryTraleInstance;
import org.kahina.tralesld.data.workbench.FeatureWorkbench;
import org.kahina.tralesld.event.TraleSLDEventTypes;

public class FeatureWorkbenchView extends KahinaView<FeatureWorkbench>
{
	private List<String> nameList;
	
	private AuxiliaryTraleInstance trale;
	
	public FeatureWorkbenchView(KahinaController control, AuxiliaryTraleInstance trale)
	{
		super(control);
		this.trale = trale;
	}
	
	public AuxiliaryTraleInstance getTrale() 
	{
		return trale;
	}

	public void setTrale(AuxiliaryTraleInstance trale) 
	{
		this.trale = trale;
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
		FeatureWorkbenchViewPanel panel = new FeatureWorkbenchViewPanel(trale);
        control.registerListener("redraw", panel);
        control.registerListener(TraleSLDEventTypes.FS_EDITOR_MESSAGE, panel);
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
		String theoryFile = model.getTheoryFileName();
		if (theoryFile == null)
		{
			return "none (theory not specified or not yet loaded)";
		}
		return theoryFile;
	}
}