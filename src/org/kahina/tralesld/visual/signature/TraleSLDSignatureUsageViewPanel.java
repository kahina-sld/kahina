package org.kahina.tralesld.visual.signature;

import javax.swing.BoxLayout;
import javax.swing.JEditorPane;

import org.kahina.core.KahinaInstance;
import org.kahina.core.visual.KahinaViewPanel;

public class TraleSLDSignatureUsageViewPanel  extends KahinaViewPanel<TraleSLDSignatureUsageView>
{

	private static final long serialVersionUID = 6789098188340983733L;
	private JEditorPane htmlPane;
	
	public TraleSLDSignatureUsageViewPanel(KahinaInstance<?, ?, ?, ?> kahina)
	{
		this.setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
		
		htmlPane = new JEditorPane();
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
