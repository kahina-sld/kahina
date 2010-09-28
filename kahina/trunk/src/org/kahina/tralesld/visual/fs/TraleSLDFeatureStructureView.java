package org.kahina.tralesld.visual.fs;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.KahinaRunner;
import org.kahina.core.visual.KahinaView;
import org.kahina.tralesld.data.fs.TraleSLDFS;

public class TraleSLDFeatureStructureView extends KahinaView<TraleSLDFS>
{
    public JComponent wrapInPanel()
    {
        TraleSLDFeatureStructureViewPanel panel = new TraleSLDFeatureStructureViewPanel();
        KahinaRunner.getControl().registerListener("redraw", panel);
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
