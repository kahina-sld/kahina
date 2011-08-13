package org.kahina.tralesld.visual.signature;

import javax.swing.JEditorPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.visual.KahinaViewPanel;

public class TraleSLDSignatureAppropriatenessViewPanel extends KahinaViewPanel<TraleSLDSignatureAppropriatenessView> implements KahinaListener
{
	private JEditorPane htmlPane;
	String currentType = "bot";
	
	public TraleSLDSignatureAppropriatenessViewPanel(KahinaController control)
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
}