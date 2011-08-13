package org.kahina.tralesld.visual.signature;

import javax.swing.JEditorPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.core.visual.chart.KahinaChartView;

public class TraleSLDSignatureUsageViewPanel  extends KahinaViewPanel<TraleSLDSignatureUsageView>
{
	private JEditorPane htmlPane;
	
	public TraleSLDSignatureUsageViewPanel(KahinaController control)
	{
		htmlPane = new JEditorPane();
		htmlPane.setContentType("text/html");
		htmlPane.setEditable(false);
		
		add(htmlPane);
	}
	
	@Override
	public void updateDisplay() 
	{
		htmlPane.setText(view.getHTML(view.getCurrentType()));		
	}
}
