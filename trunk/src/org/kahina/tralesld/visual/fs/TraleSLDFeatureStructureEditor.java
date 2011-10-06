package org.kahina.tralesld.visual.fs;

import java.awt.event.MouseListener;

import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * first attempt at minimally invasive editor layer on FS visualization
 * 
 * @author jd
 *
 */

public class TraleSLDFeatureStructureEditor extends TraleSLDFeatureStructureViewPanel
{
	public TraleSLDFeatureStructureEditor()
	{
		super();	
	}
	
	@Override
	public void updateDisplay()
	{
		innerPanel.removeAll();
		String grisuMessage;
		if (view == null || (grisuMessage = view.getGrisuMessage()) == null)
		{
			innerPanel.add(new JLabel("No feature structures (yet) at this port."));
		} 
		else
		{
			JPanel blockCanvas = util.visualize(grisuMessage).getCanvas();
			blockCanvas.addMouseListener(new TraleSLDFeatureStructureEditorMouseListener());
			innerPanel.add(blockCanvas);
		}
		innerPanel.repaint();
	}
}
