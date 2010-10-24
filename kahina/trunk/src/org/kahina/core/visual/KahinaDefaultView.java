package org.kahina.core.visual;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.KahinaObject;

public class KahinaDefaultView extends KahinaView<KahinaObject>
{
    
    public KahinaDefaultView(KahinaController control)
	{
		super(control);
	}

	public JComponent wrapInPanel(KahinaController control)
    {
        KahinaDefaultViewPanel panel = new KahinaDefaultViewPanel();
        control.registerListener("redraw", panel);
        panel.setView(this);
        return new JScrollPane(panel);
    }
}
