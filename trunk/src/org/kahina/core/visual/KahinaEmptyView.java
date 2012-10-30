package org.kahina.core.visual;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.KahinaObject;

public class KahinaEmptyView extends KahinaView<KahinaObject>
{
    public String displayString = null;
    
    public KahinaEmptyView(KahinaInstance<?, ?, ?, ?> kahina)
	{
		super(kahina);
		setTitle("Empty View");
	}
    
    public void setDisplayString(String displayString)
    {
        this.displayString = displayString;
    }
    
    public String getDisplayString()
    {
        return displayString;
    }

	@Override
	public JComponent makePanel()
    {
        KahinaEmptyViewPanel panel = new KahinaEmptyViewPanel();
        kahina.registerInstanceListener("redraw", panel);
        panel.setView(this);
        return new JScrollPane(panel);
    }
}
