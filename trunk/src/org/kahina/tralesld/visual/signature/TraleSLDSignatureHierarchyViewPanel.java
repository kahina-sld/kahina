package org.kahina.tralesld.visual.signature;

import javax.swing.BoxLayout;
import javax.swing.JTextPane;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.KahinaViewPanel;

public class TraleSLDSignatureHierarchyViewPanel extends KahinaViewPanel<TraleSLDSignatureHierarchyView>
{
	private static final long serialVersionUID = 5154672597479209987L;
	private JTextPane htmlPane;
	
	public TraleSLDSignatureHierarchyViewPanel(KahinaInstance<?, ?, ?> kahina)
	{		
		this.setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
		
		htmlPane = new JTextPane();
		htmlPane.setContentType("text/html");
		htmlPane.setEditable(false);
		htmlPane.addHyperlinkListener(new TraleSLDSignatureHyperlinkListener(kahina));
		
		add(htmlPane);
	}
	
	@Override
	public void updateDisplay() 
	{
		htmlPane.setText(view.getHTML(view.getCurrentType()));		
	}
}