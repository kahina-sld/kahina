package org.kahina.tralesld.visual.fs;

import gralej.blocks.Block;
import gralej.blocks.BlockPanel;
import gralej.om.IEntity;
import gralej.om.IType;
import gralej.om.ITypedFeatureStructure;
import gralej.parsers.IDataPackage;
import gralej.parsers.OutputFormatter;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseListener;
import java.util.Set;

import javax.swing.JLabel;
import javax.swing.JPanel;

import org.kahina.tralesld.TraleSLDState;
import org.kahina.tralesld.data.signature.TraleSLDSignature;

import com.sun.org.apache.xerces.internal.impl.xpath.regex.ParseException;

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
	
	String grisuString;
	IDataPackage data;
	
	Block contextBlock;
	IEntity contextStructure;
	String contextStructureType;
	
	public TraleSLDFeatureStructureEditor(TraleSLDState state)
	{
		super();
		
		blockPanel = null;
		
		this.state = state;
		this.sig = state.getSignature();
		
		this.grisuString = null;
		this.data = null;
		
		this.contextBlock = null;
		this.contextStructure = null;
		this.contextStructureType = "?";
	}
	
	/**
	 * Display a feature structure directly, shortcutting FS compression.
	 * This also works when no view is set.
	 * @param grisuString a feature structure in GRISU format
	 */
	public void loadGrisu(String grisuString)
	{
		this.grisuString = grisuString;
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
		if (view == null)
		{
			if (grisuString == null)
			{
				innerPanel.add(new JLabel("No feature structures (yet) at this port."));
			}
			else
			{
				try
				{
					data = util.parseGrisu(grisuString);
				}
				catch (gralej.parsers.ParseException e) 
				{
					e.printStackTrace();
					return;
				}
				blockPanel = data.createView();
				JPanel blockCanvas = blockPanel.getCanvas();
				blockCanvas.addMouseListener(new TraleSLDFeatureStructureEditorMouseListener(this, blockPanel));
				innerPanel.add(blockCanvas);
			}
		} 
		else if ((grisuString = view.getGrisuMessage()) == null)
		{
			innerPanel.add(new JLabel("No feature structures (yet) at this port."));
		}
		else
		{	
			try
			{
				data = util.parseGrisu(grisuString);
			}
			catch (gralej.parsers.ParseException e) 
			{
				e.printStackTrace();
				return;
			}
			blockPanel = data.createView();
			JPanel blockCanvas = blockPanel.getCanvas();
			blockCanvas.addMouseListener(new TraleSLDFeatureStructureEditorMouseListener(this, blockPanel));
			innerPanel.add(blockCanvas);
		}
		innerPanel.repaint();
	}
	
	public void processContextStructure(Block block)
	{
		this.contextBlock = block;
		this.contextStructure = block.getModel();
		if (contextStructure instanceof IType)
		{
			IType selectedType = (IType) contextStructure;
			contextStructureType = selectedType.typeName();
		}
		else if (contextStructure instanceof ITypedFeatureStructure)
		{
			ITypedFeatureStructure selectedFS = (ITypedFeatureStructure) contextStructure;
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
		if (contextStructure instanceof IType)
		{
			IType selectedType = (IType) contextStructure;
			selectedType.setTypeName(type);
			contextBlock.setModel(selectedType);

			//trying to get back the edited structure in GRISU format
			//TODO: find out how the data package can be manipulated via the GUI
			//data = new DataPackage();
			OutputFormatter.getInstance().save(System.err, data, blockPanel, OutputFormatter.TRALEFormat);
		}
		else if (contextStructure instanceof ITypedFeatureStructure)
		{
			ITypedFeatureStructure selectedFS = (ITypedFeatureStructure) contextStructure;
			//TODO: find out how to generate an IType object that can be used here
			selectedFS.setType(null);
		}
		//TODO: after switching the type adapt structure accordingly
		//TODO: find out how redrawing works; feed the changes back into stored date
		blockPanel.getContent().update();
		blockPanel.getCanvas().repaint();
	}
}
