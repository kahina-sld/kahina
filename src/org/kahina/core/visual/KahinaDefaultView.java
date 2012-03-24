package org.kahina.core.visual;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.KahinaObject;

public class KahinaDefaultView extends KahinaView<KahinaObject>
{
    
    public KahinaDefaultView(KahinaInstance<?, ?, ?> kahina)
	{
		super(kahina);
	}

	@Override
	public JComponent makePanel()
    {
        KahinaDefaultViewPanel panel = new KahinaDefaultViewPanel();
        kahina.getGuiControl().registerListener("redraw", panel);
        panel.setView(this);
        return new JScrollPane(panel);
    }
}
