package org.kahina.tralesld.visual.fs;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.KahinaView;
import org.kahina.tralesld.data.fs.TraleSLDFS;

public class TraleSLDFeatureStructureView extends KahinaView<TraleSLDFS>
{
	
    public TraleSLDFeatureStructureView(KahinaController control)
	{
		super(control);
	}

	public JComponent wrapInPanel(KahinaController control)
    {
        TraleSLDFeatureStructureViewPanel panel = new TraleSLDFeatureStructureViewPanel();
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
