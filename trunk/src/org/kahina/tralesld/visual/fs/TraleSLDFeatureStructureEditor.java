package org.kahina.tralesld.visual.fs;

import gralej.blocks.BlockPanel;
import gralej.om.IEntity;
import gralej.om.IType;
import gralej.om.ITypedFeatureStructure;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseListener;
import java.util.Set;

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
	
	IEntity contextStructure;
	String contextStructureType;
	
	public TraleSLDFeatureStructureEditor(TraleSLDState state)
	{
		super();
		blockPanel = null;
		this.state = state;
		this.sig = state.getSignature();
		this.contextStructure = null;
		this.contextStructureType = "?";
	}
	
	public String getContextStructureType()
	{
		return contextStructureType;
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
	
	public void processContextStructure(IEntity entity)
	{
		this.contextStructure = entity;
		if (entity instanceof IType)
		{
			IType selectedType = (IType) entity;
			contextStructureType = selectedType.typeName();
		}
		else if (entity instanceof ITypedFeatureStructure)
		{
			ITypedFeatureStructure selectedFS = (ITypedFeatureStructure) entity;
			contextStructureType = selectedFS.type().typeName();
		}
	}
	
	public Set<String> getContextSubtypes()
	{
		return sig.getSubtypes(contextStructureType);
	}
	
	public Set<String> getContextSupertypes()
	{
		return sig.getSupertypes(contextStructureType);
	}
	
	public Set<String> getContextSiblingTypes()
	{
		return sig.getSiblingTypes(contextStructureType);
	}
	
	public TraleSLDFeatureStructureEditorMenu createContextMenu()
	{
		return new TraleSLDFeatureStructureEditorMenu(this, getContextSubtypes(), getContextSupertypes(), getContextSiblingTypes());
	}

	@Override
	//only type of action at the moment are the type manipulation instructions from context menu
	//action commands are therefore simply type names; might have to be extended in the future
	public void actionPerformed(ActionEvent e) 
	{
		String type = e.getActionCommand();
		//TODO: take the cached structure and switch its type; adapt structure accordingly
		if (contextStructure instanceof IType)
		{
			IType selectedType = (IType) contextStructure;
			contextStructureType = selectedType.typeName();
		}
		else if (contextStructure instanceof ITypedFeatureStructure)
		{
			ITypedFeatureStructure selectedFS = (ITypedFeatureStructure) contextStructure;
			//TODO: find out how to generate a good type
			selectedFS.setType(null);
		}
	}
}
