package org.kahina.tralesld.visual.signature;

import javax.swing.BoxLayout;
import javax.swing.JTextPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.KahinaViewPanel;

public class TraleSLDSignatureHierarchyViewPanel extends KahinaViewPanel<TraleSLDSignatureHierarchyView>
{
	private JTextPane htmlPane;
	
	public TraleSLDSignatureHierarchyViewPanel(KahinaController control)
	{		
		this.setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
		
		htmlPane = new JTextPane();
		htmlPane.setContentType("text/html");
		htmlPane.setEditable(false);
		htmlPane.addHyperlinkListener(new TraleSLDSignatureHyperlinkListener(control));
		
		add(htmlPane);
	}
	
	@Override
	public void updateDisplay() 
	{
		htmlPane.setText(view.getHTML(view.getCurrentType()));		
	}
}