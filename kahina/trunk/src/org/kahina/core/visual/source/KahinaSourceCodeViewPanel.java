package org.kahina.core.visual.source;

import org.kahina.core.visual.text.KahinaTextViewPanel;

public class KahinaSourceCodeViewPanel extends KahinaTextViewPanel
{
	private static final long serialVersionUID = 8239151024120580599L;
    
    public void updateDisplay()
    {
        //System.err.println("Updating source code view panel!");
        //System.err.println("Old list model length: " + list.getModel().getSize());
        if (list.getModel() != view.getListModel())
        {
            list.setModel(view.getListModel());
            list.setSelectionModel(view.getSelectionModel());
            //System.err.println("New list model length: " + list.getModel().getSize());
        }
        super.updateDisplay();
    }
}
