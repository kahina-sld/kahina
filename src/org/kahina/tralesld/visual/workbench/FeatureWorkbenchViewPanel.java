package org.kahina.tralesld.visual.workbench;

import gralej.parsers.EntityFactory;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Collections;
import java.util.Enumeration;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JList;
import javax.swing.MenuSelectionManager;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.DefaultListModel;

import org.gjt.sp.jedit.bsh.This;
import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaController;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.event.KahinaPerspectiveEvent;
import org.kahina.core.event.KahinaWindowEvent;
import org.kahina.core.event.KahinaWindowEventType;
import org.kahina.core.gui.KahinaWindow;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.io.util.FileUtilities;
import org.kahina.core.io.util.XMLUtilities;
import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.tralesld.TraleSLDState;
import org.kahina.tralesld.bridge.AuxiliaryTraleInstance;
import org.kahina.tralesld.data.FeatureWorkbench;
import org.kahina.tralesld.data.fs.TraleSLDFS;
import org.kahina.tralesld.data.fs.TraleSLDPackedFSTerminal;
import org.kahina.tralesld.data.signature.TraleSLDSignature;
import org.kahina.tralesld.event.TraleSLDEventTypes;
import org.kahina.tralesld.event.TraleSLDFeatureEditEvent;
import org.kahina.tralesld.visual.fs.TraleSLDFeatureStructureEditor;
import org.kahina.tralesld.visual.fs.TraleSLDFeatureStructureEditorMenu;
import org.kahina.tralesld.visual.fs.TraleSLDFeatureStructureView;
import org.kahina.tralesld.visual.fs.VisualizationUtility;
import org.kahina.tralesld.visual.signature.TraleSLDSignatureAppropriatenessView;
import org.kahina.tralesld.visual.signature.TraleSLDSignatureHierarchyView;
import org.kahina.tralesld.visual.signature.TraleSLDSignatureUsageView;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.sun.xml.internal.messaging.saaj.packaging.mime.internet.ParseException;

/**
 * A feature workbench window, with list of objects on the left, 
 * a toolbar with functionality such as unification and diff, 
 * and an editing component on the right.
 * 
 * @author jdellert
 *
 */
public class FeatureWorkbenchViewPanel extends KahinaViewPanel<FeatureWorkbenchView> implements ListSelectionListener, ActionListener, MouseListener
{
	private final JLabel msgLabel;
	private final JLabel signatureFileLabel;
	private final JLabel theoryFileLabel;
	
	private JMenuItem exportSelectionItem;
	private JMenuItem loadSignatureItem;
	private JMenuItem compileTheoryItem;
	private JMenuItem discardTheoryItem;
	private JMenuItem reloadSignatureItem;
	private JMenuItem recompileTheoryItem;
	private JMenu fsMenu;
	private JMenu newTypeInstanceMenu;
	private JMenu newLexiconInstanceMenu;
	private JMenuItem exportFSItem;
	private JMenuItem theoryMGSItem;
	private JMenuItem sigMGUItem;
	private JMenuItem theMGUItem;
	
	private final JList list;
	private final TraleSLDFeatureStructureEditor editor;
	
	//buffered structure for copy & paste
	private String bufferedStructure = null;

	public FeatureWorkbenchViewPanel(AuxiliaryTraleInstance trale)
	{		
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		
		JMenuBar menuBar = new JMenuBar();
		menuBar.setMinimumSize(new Dimension(200,18));
		menuBar.setPreferredSize(new Dimension(500,18));
		menuBar.setMaximumSize(new Dimension(2000,18));
		
		JMenu workbenchMenu = new JMenu("Workbench");
		
		JMenuItem newWorkbenchItem = new JMenuItem("New Workbench");
		newWorkbenchItem.addActionListener(this);
		workbenchMenu.add(newWorkbenchItem);
		
		JMenuItem importWorkbenchItem = new JMenuItem("Load Workbench");
		importWorkbenchItem.addActionListener(this);
		workbenchMenu.add(importWorkbenchItem);
		
		JMenuItem exportWorkbenchItem = new JMenuItem("Save Workbench");
		exportWorkbenchItem.addActionListener(this);
		workbenchMenu.add(exportWorkbenchItem);
		
		exportSelectionItem = new JMenuItem("Export Selection to Workbench");
		exportSelectionItem.setEnabled(false);
		exportSelectionItem.addActionListener(this);
		workbenchMenu.add(exportSelectionItem);
		
		workbenchMenu.addSeparator();
		
		loadSignatureItem = new JMenuItem("Load Signature ...");
		loadSignatureItem.addActionListener(this);
		workbenchMenu.add(loadSignatureItem);
		
		compileTheoryItem = new JMenuItem("Compile Theory ...");
		compileTheoryItem.addActionListener(this);
		workbenchMenu.add(compileTheoryItem);
		
		discardTheoryItem = new JMenuItem("Discard Theory");
		discardTheoryItem.setEnabled(false);
		workbenchMenu.add(discardTheoryItem);
		
		workbenchMenu.addSeparator();
		
		reloadSignatureItem = new JMenuItem("Reload Signature");
		reloadSignatureItem.setEnabled(false);
		reloadSignatureItem.addActionListener(this);
		workbenchMenu.add(reloadSignatureItem);
		
		recompileTheoryItem = new JMenuItem("Recompile Theory");
		recompileTheoryItem.setEnabled(false);
		recompileTheoryItem.addActionListener(this);
		workbenchMenu.add(recompileTheoryItem);
		
		menuBar.add(workbenchMenu);
		
		fsMenu = new JMenu("Feature Structure");
		
		newTypeInstanceMenu = new JMenu("Add minimal FS of type ...");
		fsMenu.add(newTypeInstanceMenu);
		
		newLexiconInstanceMenu = new JMenu("Add FS for lexical entry ...");
		newLexiconInstanceMenu.setEnabled(false);
		fsMenu.add(newLexiconInstanceMenu);
		
		fsMenu.addSeparator();
		
		JMenuItem fsFileItem = new JMenuItem("Add FS from GRISU file");
		fsFileItem.setEnabled(true);
		fsFileItem.addActionListener(this);
		fsMenu.add(fsFileItem);
		
		exportFSItem = new JMenuItem("Save current FS to GRISU file");
		exportFSItem.setEnabled(false);
		exportFSItem.addActionListener(this);
		fsMenu.add(exportFSItem);
		
		fsMenu.addSeparator();
		
		theoryMGSItem = new JMenuItem("Add MGS according to theory");
		theoryMGSItem.setEnabled(false);
		fsMenu.add(theoryMGSItem);
		
		sigMGUItem = new JMenuItem("Add MGU (signature only)");
		sigMGUItem.setEnabled(false);
		fsMenu.add(sigMGUItem);
		
		theMGUItem = new JMenuItem("Add MGU according to theory");
		theMGUItem.setEnabled(false);
		fsMenu.add(theMGUItem);
		
		menuBar.add(fsMenu);
		
		menuBar.setAlignmentX(LEFT_ALIGNMENT);
		this.add(menuBar);
		
		JPanel infoPanel = new JPanel();
		infoPanel.setLayout(new BoxLayout(infoPanel, BoxLayout.Y_AXIS));
		
		signatureFileLabel = new JLabel("Signature file: none (signature not specified or not yet loaded)");
		signatureFileLabel.setAlignmentX(LEFT_ALIGNMENT);
		infoPanel.add(signatureFileLabel);
		
		theoryFileLabel = new JLabel("Theory file: none (theory not specified or not yet loaded)");
		theoryFileLabel.setAlignmentX(LEFT_ALIGNMENT);
		infoPanel.add(theoryFileLabel);
		
		infoPanel.setAlignmentX(LEFT_ALIGNMENT);
		this.add(infoPanel);
		
		JPanel messagePanel = new JPanel();
		messagePanel.setLayout(new BoxLayout(messagePanel, BoxLayout.Y_AXIS));
		
		msgLabel = new JLabel("No message.");
		msgLabel.setAlignmentX(CENTER_ALIGNMENT);
		msgLabel.setOpaque(true);
		messagePanel.add(msgLabel);
		
		messagePanel.setAlignmentX(LEFT_ALIGNMENT);
		messagePanel.setBorder(BorderFactory.createLineBorder(Color.BLACK));
		messagePanel.setMinimumSize(new Dimension(200,30));
		messagePanel.setPreferredSize(new Dimension(500,30));
		messagePanel.setMaximumSize(new Dimension(2000,30));
		this.add(messagePanel);
		
		JPanel contentPanel = new JPanel();
		contentPanel.setLayout(new BoxLayout(contentPanel, BoxLayout.X_AXIS));
		
		list = new JList();
		list.getSelectionModel().addListSelectionListener(this);
		list.addMouseListener(this);
		JScrollPane listScroller = new JScrollPane(list);
		listScroller.setPreferredSize(new Dimension(250, 80));
		listScroller.setMaximumSize(new Dimension(300, 1000));
		listScroller.setAlignmentX(CENTER_ALIGNMENT);
		contentPanel.add(listScroller);
		
		editor = new TraleSLDFeatureStructureEditor(trale);
		editor.setSignature(new TraleSLDSignature());
        KahinaRunner.getGUIControl().registerListener(TraleSLDEventTypes.FS_EDITOR_MESSAGE, editor);
		JScrollPane editorScrollPane = new JScrollPane(editor);
		contentPanel.add(editorScrollPane);
		
		contentPanel.setAlignmentX(LEFT_ALIGNMENT);
		this.add(contentPanel);
	}
	
	public void loadSignature(String sigFileName)
	{
    	if (sigFileName != null)
    	{
    		TraleSLDSignature sig = view.getTrale().getSignature(sigFileName);
    		if (sig != null)
    		{
	    		view.getModel().setSignatureFileName(sigFileName);
	    		view.getModel().setSignature(sig);
	    		editor.setSignature(view.getModel().getSignature());
	    		signatureFileLabel.setText("Signature file: " + view.getSignatureFileName());
	    		newTypeInstanceMenu.removeAll();
	    		if (view.getModel().getSignature() != null)
	    		{
	    			Set<String> baseTypes = sig.getSubtypes("bot");
	    			List<String> baseTypesList = new LinkedList<String>();
	    			baseTypesList.addAll(baseTypes);
	    			Collections.sort(baseTypesList);
	    			for (String type : baseTypesList)
	    			{
	    				newTypeInstanceMenu.add(buildTypeMenu(sig,type));
	    			}
	    		}
	    		this.processEvent(new TraleSLDFeatureEditEvent("Signature loaded.", TraleSLDFeatureEditEvent.SUCCESS_MESSAGE));
    		}
    		else
    		{
	    		this.processEvent(new TraleSLDFeatureEditEvent("Signature compilation failed!", TraleSLDFeatureEditEvent.FAILURE_MESSAGE));
    		}
    	}
    	else
    	{
    		this.processEvent(new TraleSLDFeatureEditEvent("Signature file name invalid: " + sigFileName, TraleSLDFeatureEditEvent.FAILURE_MESSAGE));
    	}
	}
	
	public void compileTheory(String theFileName)
	{
    	if (theFileName != null)
    	{
    		view.getTrale().compileGrammar(theFileName);
    		this.processEvent(new TraleSLDFeatureEditEvent("Theory compiled.", TraleSLDFeatureEditEvent.SUCCESS_MESSAGE));
    	}
    	else
    	{
    		this.processEvent(new TraleSLDFeatureEditEvent("Theory file name invalid: " + theFileName, TraleSLDFeatureEditEvent.FAILURE_MESSAGE));
    	}
	}
	
	private JComponent buildTypeMenu(TraleSLDSignature sig, String type)
	{
		Set<String> subtypes = sig.getSubtypes(type);
		if (subtypes.isEmpty())
		{
			JMenuItem typeItem = new JMenuItem(type);
			typeItem.addActionListener(this);
			return typeItem;
		}
		else
		{
			JMenu subtypeMenu = new JMenu(type);
			subtypeMenu.setActionCommand(type);
			subtypeMenu.addMouseListener(this);
			List<String> subtypesList = new LinkedList<String>();
			subtypesList.addAll(subtypes);
			Collections.sort(subtypesList);
			for (String subtype : subtypesList)
			{
				subtypeMenu.add(buildTypeMenu(sig,subtype));
			}
			return subtypeMenu;
		}
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
		view.recalculate();
		list.setListData(view.getNameList().toArray());
		theoryFileLabel.setText("Theory file: " + view.getTheoryFileName());
		updateMenus();
		this.repaint();
	}
	
	private void updateMenus()
	{
		if (view.getModel().getTheoryFileName() == null)
		{
			loadSignatureItem.setEnabled(true);
			compileTheoryItem.setEnabled(true);
			discardTheoryItem.setEnabled(false);
			recompileTheoryItem.setEnabled(false);	
			if (view.getModel().getSignatureFileName() == null)
			{
				reloadSignatureItem.setEnabled(false);
			}
			else
			{
				reloadSignatureItem.setEnabled(true);
			}
		}
		else
		{
			loadSignatureItem.setEnabled(false);
			compileTheoryItem.setEnabled(true);
			discardTheoryItem.setEnabled(true);
			reloadSignatureItem.setEnabled(false);
			recompileTheoryItem.setEnabled(true);
		}
	}
	
	@Override
	public void valueChanged(ListSelectionEvent arg0) 
	{
		int selections = list.getSelectedIndices().length;
		switch (selections)
		{
			case 0:
			{
				exportSelectionItem.setEnabled(false);
				exportFSItem.setEnabled(false);
				theoryMGSItem.setEnabled(false);
				sigMGUItem.setEnabled(false);
				theMGUItem.setEnabled(false);
				break;
			}
			case 1:
			{
				exportSelectionItem.setEnabled(true);
				exportFSItem.setEnabled(true);
				if (view.getModel().getTheoryFileName() != null) theoryMGSItem.setEnabled(true);
				sigMGUItem.setEnabled(false);
				theMGUItem.setEnabled(false);
				String name = getPrimarySelectionID();
				editor.loadGrisu(view.getModel().getStructure(name));	
				editor.updateDisplay();
				break;
			}
			case 2:
			{
				exportSelectionItem.setEnabled(true);
				exportFSItem.setEnabled(false);
				theoryMGSItem.setEnabled(false);
				sigMGUItem.setEnabled(true);
				if (view.getModel().getTheoryFileName() != null) theMGUItem.setEnabled(true);
				break;
			}
			default:
			{
				exportSelectionItem.setEnabled(true);
				exportFSItem.setEnabled(false);
				theoryMGSItem.setEnabled(false);
				sigMGUItem.setEnabled(false);
				theMGUItem.setEnabled(false);
				break;
			}
		}
	}
	
	public String getPrimarySelectionID()
	{
		return (String) list.getSelectedValue();
	}
	
	public void actionPerformed(ActionEvent e)
	{
		String action = e.getActionCommand();
		if (action.equals("Rename"))
		{
        	String name = getNewName("Enter a new name for the structure.", "Rename feature structure.");
        	if (name == null || view.getModel().getStructure(name) != null)
        	{
        		this.processEvent(new TraleSLDFeatureEditEvent("Rename failed: no name specified, or new name already exists!", TraleSLDFeatureEditEvent.FAILURE_MESSAGE));
        	}
        	else
        	{
        		String structure = view.getModel().removeStructure((String) list.getSelectedValue());
        		if (structure != null)
        		{
        			view.getModel().storeStructure(name, structure);
        			this.processEvent(new TraleSLDFeatureEditEvent("Structure renamed.", TraleSLDFeatureEditEvent.SUCCESS_MESSAGE));
        		}
        		else
        		{
        			this.processEvent(new TraleSLDFeatureEditEvent("Rename failed: could not determine structure to be renamed.", TraleSLDFeatureEditEvent.FAILURE_MESSAGE));
        		}
        	}
			updateDisplay();
			list.setSelectedValue(name, true);
		}
		else if (action.equals("Remove"))
		{
			String structure = view.getModel().removeStructure((String) list.getSelectedValue());
    		if (structure != null)
    		{
    			this.processEvent(new TraleSLDFeatureEditEvent("Structure removed.", TraleSLDFeatureEditEvent.SUCCESS_MESSAGE));
    		}
    		else
    		{
    			this.processEvent(new TraleSLDFeatureEditEvent("Removal failed: could not determine structure to be removed.", TraleSLDFeatureEditEvent.FAILURE_MESSAGE));
    		}
    		updateDisplay();
		}
		else if (action.equals("Copy"))
		{
			String copiedStructure  = view.getModel().getStructure((String) list.getSelectedValue());
    		if (copiedStructure != null)
    		{
    			this.processEvent(new TraleSLDFeatureEditEvent("Structure copied.", TraleSLDFeatureEditEvent.SUCCESS_MESSAGE));
    			KahinaRunner.getGUIControl().processEvent(new TraleSLDFeatureEditEvent(copiedStructure, TraleSLDFeatureEditEvent.COPY_FS));
    		}
    		else
    		{
    			this.processEvent(new TraleSLDFeatureEditEvent("Copying failed: could not determine structure to copy.", TraleSLDFeatureEditEvent.FAILURE_MESSAGE));
    		}
		}
		else if (action.equals("Paste"))
		{
			String name = getNewName("Enter a new name for the pasted structure.", "Paste feature structure into workbench");
        	if (name == null || view.getModel().getStructure(name) != null)
        	{
        		this.processEvent(new TraleSLDFeatureEditEvent("Paste failed: no name specified, or new name already exists!", TraleSLDFeatureEditEvent.FAILURE_MESSAGE));
        	}
        	else
        	{
        		view.getModel().storeStructure(name, bufferedStructure);
        		this.processEvent(new TraleSLDFeatureEditEvent("Structure pasted.", TraleSLDFeatureEditEvent.SUCCESS_MESSAGE));
        	}
			updateDisplay();
			list.setSelectedValue(name, true);
		}
		else if (action.equals("Add FS from GRISU file"))
		{
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Load GRISU file");
            chooser.showOpenDialog(this);
            File grisuFile = chooser.getSelectedFile();
            try
            {
            	String grisuString = FileUtilities.slurpFile(grisuFile.getAbsolutePath());
            	String name = getNewName("Enter a new name for the imported feature structure.", "Load feature structure from GRISU file");
            	if (name == null || view.getModel().getStructure(name) != null)
            	{
            		this.processEvent(new TraleSLDFeatureEditEvent("Integration failed: no name specified, or new name already exists!", TraleSLDFeatureEditEvent.FAILURE_MESSAGE));
            	}
            	else
            	{
            		view.getModel().storeStructure(name, grisuString);
            		this.processEvent(new TraleSLDFeatureEditEvent("Feature structure successfully loaded.", TraleSLDFeatureEditEvent.SUCCESS_MESSAGE));
            	}
    			updateDisplay();
    			list.setSelectedValue(name, true);
            }
            catch (IOException ioe)
            {
            	this.processEvent(new TraleSLDFeatureEditEvent("ERROR: could not load GRISU file.", TraleSLDFeatureEditEvent.FAILURE_MESSAGE));
            }
		}
		else if (action.equals("Save current FS to GRISU file"))
		{
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Save FS to GRISU file");
            chooser.showSaveDialog(this);
            File grisuFile = chooser.getSelectedFile();
            try
            {
            	String grisuString = view.getModel().getStructure((String) list.getSelectedValue());
            	FileUtilities.writeStringToFile(grisuString,grisuFile.getAbsolutePath());
            	this.processEvent(new TraleSLDFeatureEditEvent("Feature structure successfully saved.", TraleSLDFeatureEditEvent.SUCCESS_MESSAGE));
            }
            catch (IOException ioe)
            {
            	this.processEvent(new TraleSLDFeatureEditEvent("ERROR: could not save to GRISU file.", TraleSLDFeatureEditEvent.FAILURE_MESSAGE));
            }
		}
		else if (action.equals("New Workbench"))
		{
			view.display(new FeatureWorkbench());
            this.processEvent(new TraleSLDFeatureEditEvent("New workbench created.", TraleSLDFeatureEditEvent.SUCCESS_MESSAGE));
            updateDisplay();
		}
		else if (action.equals("Load Workbench"))
		{
			JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Load Workbench");
            chooser.showOpenDialog(this);
            File dataFile = chooser.getSelectedFile();
            try
            {
            	Document doc = XMLUtilities.parseXMLStream(new BufferedInputStream(new FileInputStream(dataFile)), false);
            	FeatureWorkbench workbench = FeatureWorkbench.importXML(doc.getDocumentElement());
            	view.display(workbench);
            	loadSignature(view.getModel().getSignatureFileName());
            	this.processEvent(new TraleSLDFeatureEditEvent("Workbench loaded.", TraleSLDFeatureEditEvent.SUCCESS_MESSAGE));
            }
            catch (FileNotFoundException ex)
            {
            	this.processEvent(new TraleSLDFeatureEditEvent("ERROR: file not found!", TraleSLDFeatureEditEvent.FAILURE_MESSAGE));
            }
            updateDisplay();
		}
		else if (action.equals("Save Workbench"))
		{
			JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Save Workbench As");
            chooser.showSaveDialog(this);
            File dataFile = chooser.getSelectedFile();
            Element el = view.getModel().exportXML(XMLUtilities.newEmptyDocument());
            XMLUtilities.writeXML(el, dataFile.getAbsolutePath());
            this.processEvent(new TraleSLDFeatureEditEvent("Workbench saved.", TraleSLDFeatureEditEvent.SUCCESS_MESSAGE));
		}
		else if (action.equals("Export Selection to Workbench"))
		{
			FeatureWorkbench workbench = new FeatureWorkbench();
			workbench.setSignatureFileName(view.getModel().getSignatureFileName());
			workbench.setTheoryFileName(view.getModel().getTheoryFileName());
			for (Object s : list.getSelectedValues())
			{
				workbench.storeStructure((String) s, view.getModel().getStructure((String) s));
			}
			JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Save Subset Workbench As");
            chooser.showSaveDialog(this);
            File dataFile = chooser.getSelectedFile();
            Element el = workbench.exportXML(XMLUtilities.newEmptyDocument());
            XMLUtilities.writeXML(el, dataFile.getAbsolutePath());
            this.processEvent(new TraleSLDFeatureEditEvent("Selection exported.", TraleSLDFeatureEditEvent.SUCCESS_MESSAGE));
		}
		else if (action.equals("Load Signature ..."))
		{
			JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Load Signature");
            chooser.showOpenDialog(this);
            File dataFile = chooser.getSelectedFile();
            loadSignature(dataFile.getAbsolutePath());
		}
		else if (action.equals("Compile Theory ..."))
		{
			JFileChooser chooser = new JFileChooser(new File("."));
            chooser.setDialogTitle("Compile Theory");
            chooser.showOpenDialog(this);
            File dataFile = chooser.getSelectedFile();
            compileTheory(dataFile.getAbsolutePath());
		}
		else if (action.equals("Reload Signature"))
		{
            loadSignature(view.getModel().getSignatureFileName());
            //System.err.println(view.getModel().getSignature().graphViz());
		}
		else
		{
			//default: interpret action command as type ID
			addTypeMGS(action);
		}
	}
	
	public void addTypeMGS(String type)
	{
		//the good (or bad) old way via the AuxiliaryTraleInstance
		//String result = view.getTrale().descToMgsGrisu(type);
		//the newer variant via direct GRISU computation
		//String result = "!newdata\"current\"" + view.getModel().getSignature().computeGrisuMGS(type) + "\n";
		
		//the most recent and most general way via IEntity construction
		String result = VisualizationUtility.convertGraleJToGrisu(editor.generateSignatureMGS(type, EntityFactory.getInstance()));
		if (result.startsWith("ERROR"))
		{
        	this.processEvent(new TraleSLDFeatureEditEvent(result, TraleSLDFeatureEditEvent.FAILURE_MESSAGE));
		}
		else
		{
        	this.processEvent(new TraleSLDFeatureEditEvent("Type MGS successfully added.", TraleSLDFeatureEditEvent.SUCCESS_MESSAGE));
    		view.getModel().storeStructure("mgs:" + type, result);
    		updateDisplay();
    		list.setSelectedValue("mgs:" + type, true);
		}
	}
	
	public void processEvent(KahinaEvent event)
	{
		super.processEvent(event);
		if (event instanceof TraleSLDFeatureEditEvent)
		{
			TraleSLDFeatureEditEvent editEvent = (TraleSLDFeatureEditEvent) event;
			switch (editEvent.getMessageType())
			{
				case TraleSLDFeatureEditEvent.INFO_MESSAGE:
				{
					msgLabel.setBackground(Color.WHITE);
					msgLabel.setText(editEvent.getEditMessage());
					break;
				}
				case TraleSLDFeatureEditEvent.SUCCESS_MESSAGE:
				{
					msgLabel.setBackground(Color.GREEN);
					msgLabel.setText(editEvent.getEditMessage());
					break;
				}
				case TraleSLDFeatureEditEvent.SUCCESS:
				{
					msgLabel.setBackground(Color.GREEN);
					view.getModel().storeStructure(getPrimarySelectionID(), editor.getGrisuString());
					msgLabel.setText(editEvent.getEditMessage());
					break;
				}
				case TraleSLDFeatureEditEvent.FAILURE_MESSAGE:
				{
					msgLabel.setBackground(Color.RED);
					msgLabel.setText(editEvent.getEditMessage());
					break;
				}
				case TraleSLDFeatureEditEvent.WARNING_MESSAGE:
				{
					msgLabel.setBackground(Color.YELLOW);
					msgLabel.setText(editEvent.getEditMessage());
					break;
				}
				case TraleSLDFeatureEditEvent.COPY_FS:
				{
					bufferedStructure = editEvent.getEditMessage();
					break;
				}
			}
			this.repaint();
		}
	}
	
	private String getNewName(String description, String dialogTitle)
	{
		return (String) JOptionPane.showInputDialog(this,
                description,
                dialogTitle,
                JOptionPane.PLAIN_MESSAGE);
	}

	@Override
	public void mouseClicked(MouseEvent e) 
	{
		
		if (e.getComponent() == list && e.getButton() == MouseEvent.BUTTON3)
		{
			//generate context menu for type manipulation
			FeatureWorkbenchContextMenu menu = new FeatureWorkbenchContextMenu(this);
			menu.show(e.getComponent(),e.getX(), e.getY());
			//make sure (somewhat inefficient, but does not seem to be a problem)
			int listX = e.getX() - list.getX();
			int listY = e.getY() - list.getY();
			list.setSelectedIndex(list.locationToIndex(new Point(listX,listY)));
		}
		else if (e.getComponent() instanceof JMenu)
		{
			//Swing hack: hide the menu even though no JMenuItem was clicked
			MenuSelectionManager.defaultManager().clearSelectedPath();
			
			String type = ((JMenu) e.getComponent()).getText();
			addTypeMGS(type);
		}
	}

	@Override
	public void mouseEntered(MouseEvent arg0) {
		
	}

	@Override
	public void mouseExited(MouseEvent arg0) {
		
	}

	@Override
	public void mousePressed(MouseEvent arg0) {
		
	}

	@Override
	public void mouseReleased(MouseEvent arg0) {
		
	}
	
	public static void main(String[] args)
	{
		KahinaController control = new KahinaController();
		KahinaRunner.setControl(control);
		KahinaRunner.setGUIController(control);
		AuxiliaryTraleInstance trale = new AuxiliaryTraleInstance(true);
		trale.start();
		FeatureWorkbench workbench = new FeatureWorkbench();
		workbench.setSignatureFileName(System.getProperty("user.dir") + "/signature");
		workbench.setSignature(trale.getSignature(System.getProperty("user.dir") + "/signature"));
		workbench.getSignature().inferCachedInformation();

		KahinaWindowManager wManager = new KahinaWindowManager(control);
		
		//generate a signature view window
		TraleSLDSignatureAppropriatenessView appro = new TraleSLDSignatureAppropriatenessView(control);
		appro.display(workbench.getSignature());
		TraleSLDSignatureHierarchyView hiera = new TraleSLDSignatureHierarchyView(control);
		hiera.display(workbench.getSignature());
		TraleSLDSignatureUsageView usage = new TraleSLDSignatureUsageView(control);
		usage.display(workbench.getSignature());
		
		KahinaWindow hieraWindow = wManager.integrateInDefaultWindow(hiera);
		hieraWindow.setTitle("Type hierarchy");
		KahinaWindow approWindow = wManager.integrateInDefaultWindow(appro);
		approWindow.setTitle("Appropriateness");
		KahinaWindow upperWindow = wManager.integrateInHorizontallySplitWindow(hieraWindow.getID(), approWindow.getID(), "Hierarchy & Appropriateness", control);
		upperWindow.setBorder(false);
		KahinaWindow usageWindow = wManager.integrateInDefaultWindow(usage);
		usageWindow.setTitle("Usage");
		KahinaWindow signatureWindow = wManager.integrateInVerticallySplitWindow(upperWindow.getID(), usageWindow.getID(), "Signature Inspection", control);
		signatureWindow.setBorder(false);
		signatureWindow.setSize(800, 500);
		signatureWindow.setLocation(400, 300);
		signatureWindow.setVisible(true);
		
		//generate the main feature workbench window
		FeatureWorkbenchView workbenchView = new FeatureWorkbenchView(control, trale);
		workbenchView.setTitle("Feature Workbench");
		workbenchView.display(workbench);
		
		KahinaWindow workbenchWindow = wManager.integrateInDefaultWindow(workbenchView);
		workbenchWindow.setBorder(false);
		wManager.registerWindow(workbenchWindow);
		wManager.displayWindows();
		workbenchWindow.setSize(800, 500);
		workbenchWindow.setLocation(200, 100);
		workbenchWindow.setVisible(true);
	}
}
