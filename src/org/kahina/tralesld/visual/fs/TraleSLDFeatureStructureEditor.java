package org.kahina.tralesld.visual.fs;

import gralej.blocks.Block;
import gralej.blocks.BlockPanel;
import gralej.om.Entities;
import gralej.om.IEntity;
import gralej.om.IFeatureValuePair;
import gralej.om.IType;
import gralej.om.ITypedFeatureStructure;
import gralej.parsers.IDataPackage;
import gralej.parsers.OutputFormatter;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseListener;
import java.util.HashSet;
import java.util.Set;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import org.kahina.core.KahinaRunner;
import org.kahina.tralesld.TraleSLDState;
import org.kahina.tralesld.bridge.AuxiliaryTraleInstance;
import org.kahina.tralesld.data.signature.TraleSLDSignature;
import org.kahina.tralesld.event.TraleSLDFeatureEditEvent;

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
	
	TraleSLDSignature sig;
	
	String grisuString;
	IDataPackage data;
	
	Block contextBlock;
	IEntity contextStructure;
	String contextStructureType;
	
	Block contextAttrBlock;
	IEntity contextAttrModel;
	String contextAttr;
	
	Block contextParentBlock;
	IEntity contextParentStructure;
	String contextParentStructureType;
	
	AuxiliaryTraleInstance trale;
	
	public TraleSLDFeatureStructureEditor(AuxiliaryTraleInstance trale)
	{
		super();
		
		blockPanel = null;
		
		this.sig = null;
		
		this.grisuString = null;
		this.data = null;
		
		this.contextBlock = null;
		this.contextStructure = null;
		this.contextStructureType = "?";
		
		this.contextAttrBlock = null;
		this.contextAttrModel = null;
		this.contextAttr = "?";
		
		this.contextParentBlock = null;
		this.contextParentStructure = null;
		this.contextParentStructureType = "?";
		
		this.trale = trale;
	}
	
	public void setSignature(TraleSLDSignature sig)
	{
		this.sig = sig;
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
	
	public String getContextParentStructureType()
	{
		return contextParentStructureType;
	}
	
	@Override
	public void updateDisplay()
	{
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
		contextBlock = block;
		contextStructure = block.getModel();
		contextStructureType = determineType(contextStructure);
		
		contextAttrBlock = block.getParent();
		contextAttrModel = block.getModel();
		contextAttr = determineType(contextAttrModel);
		
		contextParentBlock = contextAttrBlock.getParent();
		if (contextParentBlock == null)
		{
			contextParentStructure = null;
			contextParentStructureType = "?";
		}
		else
		{
			contextParentStructure = contextParentBlock.getModel();
			contextParentStructureType = determineType(contextParentStructure);
		}
	}
	
	private String determineType(IEntity ent)
	{
		String type = "?";
		if (ent instanceof IType)
		{
			IType selectedType = (IType) ent;
			type = selectedType.typeName();
		}
		else if (ent instanceof ITypedFeatureStructure)
		{
			ITypedFeatureStructure selectedFS = (ITypedFeatureStructure) ent;
			type = selectedFS.type().typeName();
		}
		else if (ent instanceof IFeatureValuePair)
		{
			IFeatureValuePair selectedAttr = (IFeatureValuePair) ent;
			type = selectedAttr.feature();
		}
		//the way to deal with mgsat(Type) for the moment
		if (type.startsWith("mgsat("))
		{
			type = type.substring(6, type.length() - 1);
		}
		return type;
	}
	
	public Set<String> getContextSubtypes()
	{
		if (sig == null) return null;
		return sig.getSubtypes(contextStructureType);
	}
	
	public Set<String> getContextSupertypes()
	{
		if (sig == null) return null;
		Set<String> possSupertypes = new HashSet<String>();
		for (String supertype : sig.getSupertypes(contextStructureType))
		{
			possSupertypes.add(supertype);
			//sig.getAppropriateness(type);
		}
		return possSupertypes;
	}
	
	public Set<String> getContextSiblingTypes()
	{
		if (sig == null) return null;
		return sig.getSiblingTypes(contextStructureType);
	}
	
	public TraleSLDFeatureStructureEditorMenu createContextMenu()
	{
		Set<String> subtypes = getContextSubtypes();
		Set<String> supertypes = getContextSupertypes();
		Set<String> siblingTypes = getContextSiblingTypes();
		if (sig == null)
		{
			warningMessage("No signature loaded. Cannot edit.");
			return null;
		}
		else if (supertypes == null)
		{
			failureMessage("No info on this type in signature, cannot edit.");
			return null;
		}
		else
		{
			infoMessage("Modifying structure " + contextParentStructureType + ":" + contextAttr + ":" + contextStructureType);
			return new TraleSLDFeatureStructureEditorMenu(this, subtypes, supertypes, siblingTypes);
		}
	}
	
	private void infoMessage(String desc)
	{
		KahinaRunner.getGUIControl().processEvent(new TraleSLDFeatureEditEvent(desc, TraleSLDFeatureEditEvent.INFO_MESSAGE));
	}
	
	private void successMessage(String desc)
	{
		KahinaRunner.getGUIControl().processEvent(new TraleSLDFeatureEditEvent(desc, TraleSLDFeatureEditEvent.SUCCESS_MESSAGE));
	}
	
	private void failureMessage(String desc)
	{
		KahinaRunner.getGUIControl().processEvent(new TraleSLDFeatureEditEvent(desc, TraleSLDFeatureEditEvent.FAILURE_MESSAGE));
	}
	
	private void warningMessage(String desc)
	{
		KahinaRunner.getGUIControl().processEvent(new TraleSLDFeatureEditEvent(desc, TraleSLDFeatureEditEvent.WARNING_MESSAGE));
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

			//get back the edited structure in TRALE desc format
			String traleDesc = Entities.toTraleDesc((IEntity) data.getModel());
			//use TRALE instance to retrieve the grisuString for the description's MGS

			String result = trale.descToMgsGrisu(traleDesc);
			if (result.startsWith("error"))
			{
				failureMessage(result);
			}
			else
			{
				successMessage("Editing operation successful.");
				grisuString = result;
			}
			//trale.loadEmbeddedKahinaInstance();
			
			//	failed attempt: data package cannot be manipulated via the GUI, the toTRALE-method 
			//  simply prints out the stored chars, which cannot be manipulated!
			//OutputFormatter.getInstance().save(System.err, data, blockPanel, OutputFormatter.TRALEFormat);
		}
		else if (contextStructure instanceof ITypedFeatureStructure)
		{
			ITypedFeatureStructure selectedFS = (ITypedFeatureStructure) contextStructure;
			//TODO: find out how to generate an IType object that can be used here
			selectedFS.setType(null);
		}
		//TODO: after switching the type adapt structure accordingly
		//TODO: find out how redrawing works; feed the changes back into stored date
		this.updateDisplay();
		blockPanel.getContent().update();
		blockPanel.getCanvas().repaint();
	}
}
