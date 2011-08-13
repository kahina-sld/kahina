package org.kahina.tralesld.visual.signature;

import javax.swing.JEditorPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.KahinaViewPanel;

public class TraleSLDSignatureHierarchyViewPanel extends KahinaViewPanel<TraleSLDSignatureHierarchyView>
{
	private JEditorPane htmlPane;
	
	public TraleSLDSignatureHierarchyViewPanel(KahinaController control)
	{
		htmlPane = new JEditorPane();
		htmlPane.setContentType("text/html");
		htmlPane.setEditable(false);
		htmlPane.addHyperlinkListener(new TraleSLDSignatureHyperlinkListener());
		
		add(htmlPane);
	}
	
	@Override
	public void updateDisplay() 
	{
		htmlPane.setText(view.getHTML(view.getCurrentType()));		
	}
}