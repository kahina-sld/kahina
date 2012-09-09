package org.kahina.core.visual;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.KahinaObject;

public class KahinaEmptyView extends KahinaView<KahinaObject>
{
    public KahinaEmptyView(KahinaInstance<?, ?, ?, ?> kahina)
	{
		super(kahina);
		setTitle("Empty View");
	}

	@Override
	public JComponent makePanel()
    {
        KahinaEmptyViewPanel panel = new KahinaEmptyViewPanel();
        kahina.getGuiControl().registerListener("redraw", panel);
        panel.setView(this);
        return new JScrollPane(panel);
    }
}
