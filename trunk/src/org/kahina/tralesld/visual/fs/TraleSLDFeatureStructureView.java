package org.kahina.tralesld.visual.fs;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.visual.KahinaView;
import org.kahina.tralesld.TraleSLDState;
import org.kahina.tralesld.control.TraleSLDEventTypes;
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
        TraleSLDFeatureStructureViewPanel panel = new TraleSLDFeatureStructureViewPanel();
        control.registerListener("redraw", panel);
        panel.setView(this);
        return new JScrollPane(panel);
    }
	
	public JComponent makeEditorPanel(KahinaGUI gui)
	{
		//TODO: make state handling conceptually cleaner
		TraleSLDState state = (TraleSLDState) gui.getKahinaInstance().getState();
        TraleSLDFeatureStructureEditor panel = new TraleSLDFeatureStructureEditor(gui.kahina, state.getTrale());
        panel.setSignature(state.getSignature());
        control.registerListener("redraw", panel);
        control.registerListener(TraleSLDEventTypes.FS_EDITOR_MESSAGE, panel);
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
