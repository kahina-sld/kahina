package org.kahina.tralesld.visual.signature;

import javax.swing.JEditorPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.KahinaViewPanel;

public class TraleSLDSignatureHierarchyViewPanel extends KahinaViewPanel<TraleSLDSignatureHierarchyView>
{
	private JEditorPane htmlPane;
	String currentType = "bot";
	
	public TraleSLDSignatureHierarchyViewPanel(KahinaController control)
	{
		htmlPane = new JEditorPane();
		htmlPane.setContentType("text/html");
		htmlPane.setEditable(false);
		
		add(htmlPane);
	}
	
	@Override
	public void updateDisplay() 
	{
		htmlPane.setText(view.getHTML(currentType));		
	}
	
	/**@Override
	public void processEvent(KahinaEvent event) 
	{
		if (event instanceof TraleSLDTypeSelectionEvent)
        {
            processEvent((TraleSLDTypeSelectionEvent) event);
        }
    }
    
    protected void processEvent(TraleSLDTypeSelectionEvent e)
    {
        setMarkedEdge(e.getSelectedEdge());
    }*/
}