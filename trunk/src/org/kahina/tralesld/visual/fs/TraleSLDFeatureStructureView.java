package org.kahina.tralesld.visual.fs;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.visual.KahinaView;
import org.kahina.tralesld.data.fs.TraleSLDFS;

public class TraleSLDFeatureStructureView extends KahinaView<TraleSLDFS>
{
	
    public TraleSLDFeatureStructureView(KahinaController control)
	{
		super(control);
	}

	@Override
	public JComponent makePanel(KahinaGUI gui)
    {
		//TODO: do not make all feature structures editable by default
		//should probably introduce distinction like wrapInEditablePanel vs. wrapInPanel
        TraleSLDFeatureStructureViewPanel panel = new TraleSLDFeatureStructureEditor();
        control.registerListener("redraw", panel);
        panel.setView(this);
        return new JScrollPane(panel);
    }

    public String getGrisuMessage()
    {
    	if (model == null)
    	{
    		return null;
    	}
        return model.toString();
    }
}
