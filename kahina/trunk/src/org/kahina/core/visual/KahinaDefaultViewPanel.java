package org.kahina.core.visual;

import java.awt.Graphics;

public class KahinaDefaultViewPanel extends KahinaViewPanel<KahinaDefaultView>
{
	private static final long serialVersionUID = -5117530919344443714L;

	public void paintComponent(Graphics canvas)
    {
        String displayString;
        if (view.model != null)
        {
            displayString = view.model.toString();
        }
        else
        {
            displayString = "no info";
        }
        canvas.drawString(displayString, 50, 50);
    }
}
