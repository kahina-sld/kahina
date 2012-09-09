package org.kahina.tralesld.visual.fs;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.KahinaView;
import org.kahina.tralesld.TraleSLDState;
import org.kahina.tralesld.control.TraleSLDEventTypes;
import org.kahina.tralesld.data.fs.TraleSLDFS;

public class TraleSLDFeatureStructureView extends KahinaView<TraleSLDFS>
{
	
    public TraleSLDFeatureStructureView(KahinaInstance<?, ?, ?, ?> kahina)
	{
		super(kahina);
	}

	@Override
	public JComponent makePanel()
    {
        TraleSLDFeatureStructureViewPanel panel = new TraleSLDFeatureStructureViewPanel();
        kahina.getGuiControl().registerListener("redraw", panel);
        panel.setView(this);
        return new JScrollPane(panel);
    }
	
	public JComponent makeEditorPanel(TraleSLDState state)
	{
        TraleSLDFeatureStructureEditor panel = new TraleSLDFeatureStructureEditor(kahina, state.getTrale());
        panel.setSignature(state.getSignature());
        kahina.getGuiControl().registerListener("redraw", panel);
        kahina.getGuiControl().registerListener(TraleSLDEventTypes.FS_EDITOR_MESSAGE, panel);
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
