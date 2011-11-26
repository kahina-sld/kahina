package org.kahina.tralesld.visual.fs;

import gralej.blocks.AVMBlock;
import gralej.blocks.Block;
import gralej.blocks.BlockPanel;
import gralej.blocks.ContainerBlock;
import gralej.blocks.Label;
import gralej.blocks.ListBlock;
import gralej.blocks.ListContentBlock;
import gralej.om.Entities;
import gralej.om.EntityFactory;
import gralej.om.IAny;
import gralej.om.IEntity;
import gralej.om.IFeatureValuePair;
import gralej.om.IList;
import gralej.om.ITag;
import gralej.om.IType;
import gralej.om.ITypedFeatureStructure;
import gralej.parsers.IDataPackage;
import gralej.parsers.OutputFormatter;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseListener;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import org.kahina.core.KahinaRunner;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.tralesld.TraleSLDState;
import org.kahina.tralesld.bridge.AuxiliaryTraleInstance;
import org.kahina.tralesld.data.signature.TraleSLDSignature;
import org.kahina.tralesld.event.TraleSLDFeatureEditEvent;
import org.kahina.tralesld.event.TraleSLDTypeSelectionEvent;

import com.sun.org.apache.xerces.internal.impl.xpath.regex.ParseException;

/**
 * first attempt at minimally invasive editor layer on FS visualization
 * 
 * @author jd
 *
 */

public class TraleSLDFeatureStructureEditor extends TraleSLDFeatureStructureViewPanel implements ActionListener
{
	GraleJEditorBlockPanel blockPanel;
	
	TraleSLDSignature sig;
	
	String grisuString;
	IDataPackage data;
	
	List<String> contextPath;
	
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
	private boolean identityMode = false;
	List<String> cachedPath = new LinkedList<String>();

	AuxiliaryTraleInstance trale;
	
	//option for naive editing versus totally-well-typed editing
	int editingMode = FREE_MODE;
	public static final int FREE_MODE = 0;
	public static final int TF_MODE = 1;
	public static final int TTF_MODE = 2;
	
	public TraleSLDFeatureStructureEditor(AuxiliaryTraleInstance trale)
	{
		super();
		
		blockPanel = null;
		
		this.sig = null;
		
		this.grisuString = null;
		this.data = null;
		
		this.contextPath = null;
		
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
	
	public int getEditingMode() 
	{
		return editingMode;
	}

	public void setEditingMode(int editingMode) 
	{
		this.editingMode = editingMode;
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
				blockPanel = VisualizationUtility.buildEditor(data);
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
			blockPanel = VisualizationUtility.buildEditor(data);
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
		contextStructure = contextBlock.getModel();
		contextStructureType = GraleJUtility.getType(contextStructure);
		
		if (!contextStructureType.equals("?"))
		{
			KahinaRunner.processEvent(new TraleSLDTypeSelectionEvent(contextStructureType));
			KahinaRunner.processEvent(new KahinaRedrawEvent());
		}
		
		contextPath = determinePath(block);
		
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
				contextParentStructureType = GraleJUtility.getType(contextParentStructure);
			}
		}
	}
	
	private List<String> determinePath(Block block)
	{
		List<String> path = new LinkedList<String>();
		Block secondToLastBlock = null;
		Block lastBlock = null;
		while (block != null)
		{
			if (block.getModel() instanceof IFeatureValuePair && block instanceof ContainerBlock)
			{
				//System.err.println("  path: " + ((IFeatureValuePair) block.getModel()).feature());
				path.add(0,((IFeatureValuePair) block.getModel()).feature());
			}
			else if (block instanceof ListBlock)
			{
				ListBlock listBlock = (ListBlock) block;
				int i = 0;
				for (Block bl : listBlock.getChildren())
				{		
					//System.err.println("  bl: " + bl);
					if (bl instanceof ListContentBlock)
					{
						ListContentBlock cbl = (ListContentBlock) bl;
						for (Block bbl : cbl.getChildren())
						{
							//System.err.println("     bbl: " + bbl);
							if (bbl == secondToLastBlock)
							{
								path.add(i,"hd");
								break;
							}
							if (trueListContentBlock(bbl))
							{
								path.add(0,"tl");
								i++;
							}
						}
					}
				}
			}
			secondToLastBlock = lastBlock;
			lastBlock = block;
			block = block.getParent();
			//System.err.println("block: " + block + " lastBlock: " + lastBlock + " secondToLastBlock: " + secondToLastBlock);
		}
		return path;
	}
	
	private boolean trueListContentBlock(Block b)
	{
		if (b instanceof AVMBlock) return true;
		if (b instanceof Label)
		{
			Label l = (Label) b;
			if (l.getText().equals(",")) return false;
			return true;
		}
		return false;
	}
	
	private Block getStructureParent(Block block)
	{
		while(block != null && !(block.getModel() instanceof ITypedFeatureStructure))
		{
			block = block.getParent();
		}
		return block;
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
	
	public List<String> getContextSubtypes()
	{
		if (sig == null) return null;
		List<String> subtypes = new LinkedList<String>();
		subtypes.addAll(sig.getSubtypes(contextStructureType));
		Collections.sort(subtypes);
		return subtypes;
	}
	
	public List<String> getContextSupertypes()
	{
		if (sig == null) return null;
		Set<String> supertypes = (sig.getSupertypes(contextStructureType));
		if (supertypes == null) return null;
		List<String> possSupertypes = new LinkedList<String>();
		for (String supertype : supertypes)
		{
			if (contextParentStructureType.isEmpty() || editingMode == FREE_MODE)
			{
				possSupertypes.add(supertype);
			}
			else
			{
				String approType = sig.getAppropriateValueType(contextParentStructureType,contextAttr);
				if (sig.dominates(approType,supertype))
				{
					possSupertypes.add(supertype);
				}
			}
		}
		Collections.sort(possSupertypes);
		return possSupertypes;
	}
	
	public List<String> getContextSiblingTypes()
	{
		if (sig == null) return null;
		List<String> possSiblings = new LinkedList<String>();
		for (String sibling : sig.getSiblingTypes(contextStructureType))
		{
			if (contextParentStructureType.isEmpty() || editingMode == FREE_MODE)
			{
				possSiblings.add(sibling);
			}
			else
			{
				String approType = sig.getAppropriateValueType(contextParentStructureType,contextAttr);
				if (sig.dominates(approType,sibling))
				{
					possSiblings.add(sibling);
				}
			}
		}
		Collections.sort(possSiblings);
		return possSiblings;
	}
	
	public List<String> getContextFeatures()
	{
		List<String> features = new LinkedList<String>();
		if (editingMode == FREE_MODE)
		{
			features.addAll(sig.getFeatures());
			for (String feat : GraleJUtility.listFeatures(contextStructure))
			{
				features.remove(feat);
			}
		}
		else if (editingMode == TF_MODE)
		{
			features.addAll(sig.getAppropriateness(contextStructureType).keySet());
			for (String feat : GraleJUtility.listFeatures(contextStructure))
			{
				features.remove(feat);
			}
		}
		Collections.sort(features);
		return features;
	}
	
	
	public TraleSLDFeatureStructureEditorMenu createAppropriateContextMenu()
	{
		if (contextStructure instanceof IType || contextStructure instanceof ITypedFeatureStructure)
		{
			return createTypeContextMenu();
		}
		if (contextStructure instanceof IAny)
		{
			return createAtomContextMenu();
		}
		if (contextStructure instanceof IFeatureValuePair)
		{
			return createFeatureContextMenu();
		}
		if (contextStructure instanceof ITag)
		{
			return createTagContextMenu();
		}
		return null;
	}
	
	public TraleSLDFeatureStructureEditorMenu createTypeContextMenu()
	{
		List<String> supertypes = getContextSupertypes();
		if (sig == null)
		{
			warningMessage("No signature loaded. Cannot edit.");
			return null;
		}
		else if (supertypes == null)
		{
			return createAtomContextMenu();
		}
		else
		{
			List<String> subtypes = getContextSubtypes();
			List<String> siblingTypes = getContextSiblingTypes();
			List<String> introFeatures = getContextFeatures();
			infoMessage("Modifying structure at path: " + contextPath);
			return TraleSLDFeatureStructureEditorMenu.newTypeMenu(this, subtypes, supertypes, siblingTypes, introFeatures, editingMode, identityMode);
		}
	}
	
	public TraleSLDFeatureStructureEditorMenu createAtomContextMenu()
	{
		infoMessage("Modyfing atom at path: " + contextPath);
		return TraleSLDFeatureStructureEditorMenu.newAtomMenu(this);
	}
	
	public TraleSLDFeatureStructureEditorMenu createFeatureContextMenu()
	{
		infoMessage("Modifying feature at path: " + contextPath);
		return TraleSLDFeatureStructureEditorMenu.newFeatureMenu(this,editingMode);
	}
	
	public TraleSLDFeatureStructureEditorMenu createTagContextMenu()
	{
		infoMessage("Modifying tag at path: " + contextPath);
		return TraleSLDFeatureStructureEditorMenu.newTagMenu(this);
	}
	
	public void infoMessage(String desc)
	{
		KahinaRunner.getGUIControl().processEvent(new TraleSLDFeatureEditEvent(desc, TraleSLDFeatureEditEvent.INFO_MESSAGE));
	}
	
	public void success(String desc)
	{
		KahinaRunner.getGUIControl().processEvent(new TraleSLDFeatureEditEvent(desc, TraleSLDFeatureEditEvent.SUCCESS));
	}
	
	public void failureMessage(String desc)
	{
		KahinaRunner.getGUIControl().processEvent(new TraleSLDFeatureEditEvent(desc, TraleSLDFeatureEditEvent.FAILURE_MESSAGE));
	}
	
	public void warningMessage(String desc)
	{
		KahinaRunner.getGUIControl().processEvent(new TraleSLDFeatureEditEvent(desc, TraleSLDFeatureEditEvent.WARNING_MESSAGE));
	}

	@Override
	
	public void actionPerformed(ActionEvent e) 
	{
		String command = e.getActionCommand();
		if (command.equals("Copy"))
		{
			String copyGrisu = GraleJUtility.convertGraleJToGrisu(contextStructure);
			if (copyGrisu.startsWith("ERROR"))
			{
				failureMessage(copyGrisu);
			}
			else
			{
				success("Copying operation successful.");
				KahinaRunner.getGUIControl().processEvent(new TraleSLDFeatureEditEvent(copyGrisu, TraleSLDFeatureEditEvent.COPY_FS));
			}		
		}
		else if (command.equals("Paste"))
		{
			IDataPackage toCopyData = null;
			try
			{
				toCopyData = util.parseGrisu(bufferedStructure);
			}
			catch (gralej.parsers.ParseException pe) 
			{
				failureMessage("Paste failed: GRISU string could not be parsed.");
				return;
			}
			//TODO: check whether types are compatible
			//TODO: find out about weird behavior, and why the second option leads to parse errors	
			
			//contextBlock.setModel((IEntity) toCopyData.getModel());
			contextAttrModel.setValue((IEntity) toCopyData.getModel());
			reconvert();
		}
		else if (command.startsWith("spe:"))
		{
			String tau = command.substring(4);
			if (tau.equals("(atom)"))
			{
				String newName = (String) JOptionPane.showInputDialog(this,
						"Enter the new string (no type name)",
		                "Build Atom",
		                JOptionPane.PLAIN_MESSAGE);
				if (newName == null)
				{
					failureMessage("ERROR: No new string for the atom defined!");
				}
				else if (sig.getTypes().contains(newName))
				{
					failureMessage("ERROR: \"" + newName + "\" is a type name and therefore cannot be an atomic string.");
				}
				else
				{
					IEntity res = GraleJUtility.changeAtom((IEntity) data.getModel(), contextPath, newName, sig);
					reconvert(res);
				}
			}
			else 
			{
				if (editingMode == TTF_MODE)
				{
					GraleJUtility.specializeTTF((IEntity) data.getModel(), contextPath, tau, sig);
				}
				else
				{
					GraleJUtility.specialize((IEntity) data.getModel(), contextPath, tau, sig);
				}
				reconvert();
			}
		}
		else if (command.startsWith("gen:"))
		{
			String tau = command.substring(4);
			if (editingMode == TTF_MODE)
			{
				GraleJUtility.generalizeTTF((IEntity) data.getModel(), contextPath, tau, sig);
			}
			else
			{
				GraleJUtility.generalize((IEntity) data.getModel(), contextPath, tau, sig);
			}
			reconvert();
		}
		else if (command.startsWith("swi:"))
		{
			String tau = command.substring(4);
			if (editingMode == TTF_MODE)
			{
				GraleJUtility.switchTTF((IEntity) data.getModel(), contextPath, tau, sig);
			}
			else
			{
				GraleJUtility.switchType((IEntity) data.getModel(), contextPath, tau, sig);
			}
			reconvert();
		}
		else if (command.startsWith("fea:"))
		{
			String f = command.substring(4);
			String parentType = contextParentStructureType;
			IEntity mgs = GraleJUtility.signatureMGS(sig.getAppropriateValueType(parentType, f), sig);
			IEntity result = GraleJUtility.introFeat((IEntity) data.getModel(), contextPath, f, mgs, sig);
			reconvert(result);
		}
		else if (command.equals("Remove"))
		{
			String feat = ((IFeatureValuePair) contextStructure).feature(); 
			GraleJUtility.remFeat((IEntity) data.getModel(), contextPath, feat);
			reconvert();
		}
		else if (command.equals("Reset"))
		{
			String feat = ((IFeatureValuePair) contextStructure).feature(); 
			GraleJUtility.resetFeat((IEntity) data.getModel(), contextPath, feat, sig);
			reconvert();
		}
		else if (command.equals("Dissolve"))
		{
			GraleJUtility.remIdent((IEntity) data.getModel(), contextPath);
			reconvert();
		}
		else if (command.equals("Begin"))
		{
			identityMode = true;
			cachedPath = contextPath;
		}
		else if (command.equals("Identity"))
		{
			IEntity result = GraleJUtility.makeIdent((IEntity) data.getModel(), cachedPath, contextPath, sig);
			if (result != null)
			{
				reconvert();
			}
			identityMode = false;
		}
		else if (command.equals("ChangeAtom"))
		{
			String newName = (String) JOptionPane.showInputDialog(this,
					"Enter the new string (no type name)",
	                "Change Atom",
	                JOptionPane.PLAIN_MESSAGE);
			if (newName == null)
			{
				failureMessage("ERROR: No new string for the atom defined!");
			}
			else if (sig.getTypes().contains(newName))
			{
				failureMessage("ERROR: \"" + newName + "\" is a type name and therefore cannot be an atomic string.");
			}
			else
			{
				IEntity res = GraleJUtility.changeAtom((IEntity) data.getModel(), contextPath, newName, sig);
				reconvert(res);
			}
		}
		else if (command.equals("GezAtom"))
		{
			IEntity res = GraleJUtility.generalizeAtom((IEntity) data.getModel(), contextPath, sig);
			reconvert(res);
		}
		else //LEGACY CODE, ONLY HERE FOR REFERENCE
		{
			String type = command;
			if (contextStructure instanceof IType)
			{
				IType selectedType = (IType) contextStructure;
				selectedType.setTypeName(type);
			}
			else if (contextStructure instanceof ITypedFeatureStructure)
			{
				EntityFactory ent = EntityFactory.getInstance();
				ITypedFeatureStructure selectedFS = (ITypedFeatureStructure) contextStructure;
				selectedFS.type().setTypeName(type);
				Map<String,String> appropFeats = sig.getTypeRestrictions(type);
				List<String> appropFeatsList = new LinkedList<String>();
				appropFeatsList.addAll(appropFeats.keySet());
				Collections.sort(appropFeatsList);
				Map<String,IFeatureValuePair> featureMap = new HashMap<String,IFeatureValuePair>();
				for (IFeatureValuePair pair : selectedFS.featureValuePairs())
				{
					featureMap.put(pair.text(), pair);
				}
				//TODO: subsumption check to determine whether feature values are specific enough
				for (String feat : appropFeatsList)
				{
					IFeatureValuePair fv = featureMap.remove(feat);
					if (fv == null)
					{
						selectedFS.addFeatureValue(ent.newFeatVal(feat, GraleJUtility.signatureMGS(appropFeats.get(feat), sig)));
					}
				}
				//remove superfluous features
				for (IFeatureValuePair fv : featureMap.values())
				{
					selectedFS.featureValuePairs().remove(fv);
				}
			}
			//THE OLD WAY: editing via AuxiliaryTraleInstance
			/*//get back the edited structure in TRALE desc format
			String traleDesc = Entities.toTraleDesc((IEntity) data.getModel());
			//use TRALE instance to retrieve the grisuString for the description's MGS
			String result = trale.descToMgsGrisu(traleDesc);
			result = sig.resolveMGSs(result);*/
			//THE NEW WAY: render edited structure into grisu
			reconvert();	
			//failed attempt: data package cannot be manipulated via the GUI, the toTRALE-method 
			//simply prints out the stored chars, which cannot be manipulated!
			//OutputFormatter.getInstance().save(System.err, data, blockPanel, OutputFormatter.TRALEFormat);
		}
		this.updateDisplay();
		blockPanel.getContent().update();
		blockPanel.getCanvas().repaint();
	}
	
	private void reconvert()
	{
		String result = GraleJUtility.convertGraleJToGrisu((IEntity) data.getModel());
		if (result.startsWith("ERROR"))
		{
			failureMessage(result);
		}
		else
		{
			grisuString = result;
			KahinaRunner.getGUIControl().processEvent(new TraleSLDFeatureEditEvent(null, TraleSLDFeatureEditEvent.UPDATE_FS));
		}
	}
	
	private void reconvert(IEntity newRoot)
	{
		String result = GraleJUtility.convertGraleJToGrisu(newRoot);
		if (result.startsWith("ERROR"))
		{
			failureMessage(result);
		}
		else
		{
			grisuString = result;
			KahinaRunner.getGUIControl().processEvent(new TraleSLDFeatureEditEvent(null, TraleSLDFeatureEditEvent.UPDATE_FS));
		}
	}
}
