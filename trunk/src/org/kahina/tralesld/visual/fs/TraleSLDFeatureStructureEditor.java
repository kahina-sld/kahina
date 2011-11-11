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

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseListener;
import java.util.HashSet;
import java.util.Set;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import org.kahina.core.KahinaRunner;
import org.kahina.core.event.KahinaEvent;
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
	IFeatureValuePair contextAttrModel;
	String contextAttr;
	
	Block contextParentBlock;
	IEntity contextParentStructure;
	String contextParentStructureType;
	
	//buffered structure for copy & paste
	private String bufferedStructure = null;

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
	
	public String getGrisuString()
	{
		return grisuString;
	}
	
	public String getContextStructureType()
	{
		return contextStructureType;
	}
	
	public String getContextParentStructureType()
	{
		return contextParentStructureType;
	}
	
	public String getBufferedStructure() 
	{
		return bufferedStructure;
	}

	public void setBufferedStructure(String bufferedStructure) 
	{
		this.bufferedStructure = bufferedStructure;
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
	
	public void processEvent(KahinaEvent event)
	{
		super.processEvent(event);
		if (event instanceof TraleSLDFeatureEditEvent)
		{
			TraleSLDFeatureEditEvent editEvent = (TraleSLDFeatureEditEvent) event;
			switch (editEvent.getMessageType())
			{
				case TraleSLDFeatureEditEvent.COPY_FS:
				{
					bufferedStructure = editEvent.getEditMessage();
					break;
				}
			}
			this.repaint();
		}
	}
	
	public void processContextStructure(Block block)
	{
		contextBlock = block;
		contextStructure = block.getModel();
		contextStructureType = determineType(contextStructure);
		
		contextAttrBlock = getAttrParent(block);
		if (contextAttrBlock == null)
		{
			contextAttrModel = null;
			contextAttr = "ROOT";
			
			contextParentStructure = null;
			contextParentStructureType = "";
		}
		else
		{
			contextAttrModel = (IFeatureValuePair) contextAttrBlock.getModel();
			contextAttr = contextAttrModel.feature();
		
			contextParentBlock = getTypeParent(contextAttrBlock);
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
	}
	
	private Block getAttrParent(Block block)
	{
		while(block != null && !(block.getModel() instanceof IFeatureValuePair))
		{
			block = block.getParent();
		}
		return block;
	}
	
	private Block getTypeParent(Block block)
	{
		while(block != null && ((block.getModel() == null) || !(block.getModel() instanceof ITypedFeatureStructure)))
		{
			block = block.getParent();
		}
		return block;
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
		else
		{
			System.err.println(ent);
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
		Set<String> supertypes = (sig.getSupertypes(contextStructureType));
		if (supertypes == null) return null;
		Set<String> possSupertypes = new HashSet<String>();
		for (String supertype : supertypes)
		{
			if (contextParentStructureType.isEmpty())
			{
				possSupertypes.add(supertype);
			}
			else
			{
				String approType = sig.getAppropriateness(contextParentStructureType).get(contextAttr);
				if (sig.dominates(approType,supertype))
				{
					possSupertypes.add(supertype);
				}
			}
		}
		return possSupertypes;
	}
	
	public Set<String> getContextSiblingTypes()
	{
		if (sig == null) return null;
		Set<String> possSiblings = new HashSet<String>();
		for (String sibling : sig.getSiblingTypes(contextStructureType))
		{
			if (contextParentStructureType.isEmpty())
			{
				possSiblings.add(sibling);
			}
			else
			{
				String approType = sig.getAppropriateness(contextParentStructureType).get(contextAttr);
				if (sig.dominates(approType,sibling))
				{
					possSiblings.add(sibling);
				}
			}
		}
		return possSiblings;
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
			infoMessage("Modifying structure " + contextParentStructureType + ":" + contextAttr.toUpperCase() + ":" + contextStructureType);
			return new TraleSLDFeatureStructureEditorMenu(this, subtypes, supertypes, siblingTypes);
		}
	}
	
	private void infoMessage(String desc)
	{
		KahinaRunner.getGUIControl().processEvent(new TraleSLDFeatureEditEvent(desc, TraleSLDFeatureEditEvent.INFO_MESSAGE));
	}
	
	private void success(String desc)
	{
		KahinaRunner.getGUIControl().processEvent(new TraleSLDFeatureEditEvent(desc, TraleSLDFeatureEditEvent.SUCCESS));
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
	
	public void actionPerformed(ActionEvent e) 
	{
		String command = e.getActionCommand();
		if (command.equals("Copy"))
		{
			String traleDesc = Entities.toTraleDesc(contextStructure);
			//use TRALE instance to retrieve the grisuString for the description's MGS
			String result = trale.descToMgsGrisu(traleDesc);
			result = sig.resolveMGSs(result);
			if (result.startsWith("ERROR"))
			{
				failureMessage(result);
			}
			else
			{
				success("Copying operation successful.");
				KahinaRunner.getGUIControl().processEvent(new TraleSLDFeatureEditEvent(result, TraleSLDFeatureEditEvent.COPY_FS));
			}		
		}
		else if (command.equals("Paste"))
		{
			IDataPackage toCopyData = null;
			try
			{
				toCopyData = util.parseGrisu(grisuString);
			}
			catch (gralej.parsers.ParseException pe) 
			{
				failureMessage("Paste failed: GRISU string could not be parsed.");
				return;
			}
			//TODO: check whether types are compatible
			//TODO: find out about weird behavior, and why the second option leads to parse errors
			contextBlock.setModel((IEntity) toCopyData.getModel());
			//contextAttrModel.setValue((IEntity) toCopyData.getModel());
			//get back the edited structure in TRALE desc format
			String traleDesc = Entities.toTraleDesc((IEntity) data.getModel());
			//use TRALE instance to retrieve the grisuString for the description's MGS
			String result = trale.descToMgsGrisu(traleDesc);
			if (result.startsWith("error"))
			{
				failureMessage("Paste failed with " + result);
			}
			else
			{
				grisuString = result;
				success("Paste successful.");
			}
		}
		else
		{
			//other action commands are simply type names for type manipulation instructions from context menu
			String type = command;
			if (contextStructure instanceof IType)
			{
				IType selectedType = (IType) contextStructure;
				selectedType.setTypeName(type);
			}
			else if (contextStructure instanceof ITypedFeatureStructure)
			{
				ITypedFeatureStructure selectedFS = (ITypedFeatureStructure) contextStructure;
				selectedFS.type().setTypeName(type);
			}
			//get back the edited structure in TRALE desc format
			String traleDesc = Entities.toTraleDesc((IEntity) data.getModel());
			//use TRALE instance to retrieve the grisuString for the description's MGS
			String result = trale.descToMgsGrisu(traleDesc);
			result = sig.resolveMGSs(result);
			if (result.startsWith("ERROR"))
			{
				failureMessage(result);
			}
			else
			{
				grisuString = result;
				success("Editing operation successful.");
			}
			
			//failed attempt: data package cannot be manipulated via the GUI, the toTRALE-method 
			//simply prints out the stored chars, which cannot be manipulated!
			//OutputFormatter.getInstance().save(System.err, data, blockPanel, OutputFormatter.TRALEFormat);
		}
		this.updateDisplay();
		blockPanel.getContent().update();
		blockPanel.getCanvas().repaint();
	}
}
