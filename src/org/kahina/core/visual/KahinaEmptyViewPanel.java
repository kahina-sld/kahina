package org.kahina.core.visual;

import java.awt.Graphics;

public class KahinaEmptyViewPanel extends KahinaViewPanel<KahinaEmptyView>
{
	@Override
	public void paintComponent(Graphics canvas)
    {
        String displayString;
        if (view.model != null)
        {
            displayString = view.model.toString();
        }
        else if (view.getDisplayString() != null)
        {
            displayString = view.getDisplayString();
        }
        else
        {
            displayString = "Drag a window or frame here.";
        }
        canvas.drawString(displayString, 50, 50);
    }

	@Override
	public void updateDisplay()
	{
		// do nothing
	}
}
