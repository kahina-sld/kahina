package org.kahina.tralesld.visual.fs;

import gralej.blocks.BlockPanel;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseListener;

import javax.swing.JLabel;
import javax.swing.JPanel;

import org.kahina.tralesld.TraleSLDState;
import org.kahina.tralesld.data.signature.TraleSLDSignature;

/**
 * first attempt at minimally invasive editor layer on FS visualization
 * 
 * @author jd
 *
 */

public class TraleSLDFeatureStructureEditor extends TraleSLDFeatureStructureViewPanel implements ActionListener
{
	BlockPanel blockPanel;
	TraleSLDState state;
	TraleSLDSignature sig;
	
	public TraleSLDFeatureStructureEditor(TraleSLDState state)
	{
		super();
		blockPanel = null;
		this.state = state;
		this.sig = state.getSignature();
	}
	
	@Override
	public void updateDisplay()
	{
		//TODO: find a better solution for ensuring the signature is always up-to-date
		sig = state.getSignature();
		innerPanel.removeAll();
		String grisuMessage;
		if (view == null || (grisuMessage = view.getGrisuMessage()) == null)
		{
			innerPanel.add(new JLabel("No feature structures (yet) at this port."));
		} 
		else
		{
			blockPanel = util.visualize(grisuMessage);
			JPanel blockCanvas = blockPanel.getCanvas();
			blockCanvas.addMouseListener(new TraleSLDFeatureStructureEditorMouseListener(this, blockPanel));
			innerPanel.add(blockCanvas);
		}
		innerPanel.repaint();
	}

	@Override
	//only type of action at the moment are the type manipulation instructions from context menu
	//action commands are therefore simply type names; might have to be extended in the future
	public void actionPerformed(ActionEvent e) 
	{
		String type = e.getActionCommand();
		//TODO: take the cached structure and switch its type; adapt structure accordingly
	}
}
