package org.kahina.core.visual;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.gui.KahinaGUI;

public class KahinaEmptyView extends KahinaView<KahinaObject>
{
    public KahinaEmptyView(KahinaController control)
	{
		super(control);
		setTitle("Empty View");
	}

	@Override
	public JComponent makePanel(KahinaGUI gui)
    {
        KahinaEmptyViewPanel panel = new KahinaEmptyViewPanel();
        control.registerListener("redraw", panel);
        panel.setView(this);
        return new JScrollPane(panel);
    }
}
