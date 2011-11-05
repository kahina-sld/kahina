package org.kahina.tralesld.visual.workbench;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Enumeration;
import java.util.List;
import java.util.Set;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JList;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.DefaultListModel;

import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaController;
import org.kahina.core.event.KahinaEvent;
import org.kahina.core.event.KahinaWindowEvent;
import org.kahina.core.event.KahinaWindowEventType;
import org.kahina.core.gui.KahinaWindow;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.tralesld.TraleSLDState;
import org.kahina.tralesld.bridge.AuxiliaryTraleInstance;
import org.kahina.tralesld.data.FeatureWorkbench;
import org.kahina.tralesld.data.fs.TraleSLDFS;
import org.kahina.tralesld.data.fs.TraleSLDPackedFSTerminal;
import org.kahina.tralesld.data.signature.TraleSLDSignature;
import org.kahina.tralesld.event.TraleSLDFeatureEditEvent;
import org.kahina.tralesld.visual.fs.TraleSLDFeatureStructureEditor;
import org.kahina.tralesld.visual.fs.TraleSLDFeatureStructureEditorMenu;
import org.kahina.tralesld.visual.fs.TraleSLDFeatureStructureView;

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
	
	private JMenu newTypeInstanceMenu;
	private JMenu newLexiconInstanceMenu;
	
	private final JList list;
	private final TraleSLDFeatureStructureEditor editor;
	
	public FeatureWorkbenchViewPanel(AuxiliaryTraleInstance trale)
	{		
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		
		JMenuBar menuBar = new JMenuBar();
		menuBar.setMinimumSize(new Dimension(200,18));
		menuBar.setPreferredSize(new Dimension(500,18));
		menuBar.setMaximumSize(new Dimension(2000,18));
		
		JMenu workbenchMenu = new JMenu("Workbench");
		
		JMenuItem newWorkbenchItem = new JMenuItem("New Workbench");
		workbenchMenu.add(newWorkbenchItem);
		
		JMenuItem importWorkbenchItem = new JMenuItem("Import Workbench");
		workbenchMenu.add(importWorkbenchItem);
		
		JMenuItem exportWorkbenchItem = new JMenuItem("Export Workbench");
		workbenchMenu.add(exportWorkbenchItem);
		
		JMenuItem exportSelectionItem = new JMenuItem("Export Selection to Workbench");
		workbenchMenu.add(exportSelectionItem);
		
		workbenchMenu.addSeparator();
		
		JMenuItem loadSignatureItem = new JMenuItem("Load Signature ...");
		workbenchMenu.add(loadSignatureItem);
		
		JMenuItem compileTheoryItem = new JMenuItem("Compile Theory ...");
		workbenchMenu.add(compileTheoryItem);
		
		menuBar.add(workbenchMenu);
		
		JMenu fsMenu = new JMenu("Feature Structure");
		
		newTypeInstanceMenu = new JMenu("Add minimal FS of type ...");
		fsMenu.add(newTypeInstanceMenu);
		
		newLexiconInstanceMenu = new JMenu("Add FS for lexical entry ...");
		fsMenu.add(newLexiconInstanceMenu);
		
		JMenuItem fsClipboardItem = new JMenuItem("Paste FS from clipboard");
		fsMenu.add(fsClipboardItem);
		
		fsMenu.addSeparator();
		
		JMenuItem fsFileItem = new JMenuItem("Add FS from GRISU file");
		fsMenu.add(fsFileItem);
		
		JMenuItem exportFSItem = new JMenuItem("Save current FS to GRISU file");
		fsMenu.add(exportFSItem);
		
		fsMenu.addSeparator();
		
		JMenuItem theoryMGSItem = new JMenuItem("Add MGS according to theory");
		fsMenu.add(theoryMGSItem);
		
		JMenuItem sigMGUItem = new JMenuItem("Add MGU (signature only)");
		fsMenu.add(sigMGUItem);
		
		JMenuItem theMGUItem = new JMenuItem("Add MGU according to theory");
		fsMenu.add(theMGUItem);
		
		menuBar.add(fsMenu);
		
		menuBar.setAlignmentX(LEFT_ALIGNMENT);
		this.add(menuBar);
		
		JPanel infoPanel = new JPanel();
		infoPanel.setLayout(new BoxLayout(infoPanel, BoxLayout.Y_AXIS));
		
		signatureFileLabel = new JLabel("Signature file: ");
		signatureFileLabel.setAlignmentX(LEFT_ALIGNMENT);
		infoPanel.add(signatureFileLabel);
		
		theoryFileLabel = new JLabel("Theory file: ");
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
		JScrollPane editorScrollPane = new JScrollPane(editor);
		contentPanel.add(editorScrollPane);
		
		contentPanel.setAlignmentX(LEFT_ALIGNMENT);
		this.add(contentPanel);
	}

	@Override
	public void updateDisplay() 
	{
		editor.setSignature(view.getModel().getSignature());
		view.recalculate();
		list.setListData(view.getNameList().toArray());
		signatureFileLabel.setText("Signature file: " + view.getSignatureFileName());
		theoryFileLabel.setText("Theory file: " + view.getTheoryFileName());
		newTypeInstanceMenu.removeAll();
		if (view.getModel().getSignature() != null)
		{
			TraleSLDSignature sig = view.getModel().getSignature();
			Set<String> baseTypes = sig.getSubtypes("bot");
			for (String type : baseTypes)
			{
				newTypeInstanceMenu.add(buildTypeMenu(sig,type));
			}
		}
		this.repaint();
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
			subtypeMenu.addActionListener(this);
			for (String subtype : subtypes)
			{
				subtypeMenu.add(buildTypeMenu(sig,subtype));
			}
			return subtypeMenu;
		}
	}

	@Override
	public void valueChanged(ListSelectionEvent arg0) 
	{
		String name = getPrimarySelectionID();
		editor.loadGrisu(view.getModel().getStructure(name));	
		editor.updateDisplay();
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
		}
		else
		{
			//default: interpret action command as type ID
			String grisuString = view.getTrale().descToMgsGrisu(action);
			view.getModel().storeStructure("mgs:" + action, grisuString);
			updateDisplay();
			list.setSelectedValue("mgs:" + action, true);
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
					break;
				}
				case TraleSLDFeatureEditEvent.SUCCESS_MESSAGE:
				{
					msgLabel.setBackground(Color.GREEN);
					view.getModel().storeStructure(getPrimarySelectionID(), editor.getGrisuString());
					break;
				}
				case TraleSLDFeatureEditEvent.FAILURE_MESSAGE:
				{
					msgLabel.setBackground(Color.RED);
					break;
				}
				case TraleSLDFeatureEditEvent.WARNING_MESSAGE:
				{
					msgLabel.setBackground(Color.YELLOW);
					break;
				}
			}
			msgLabel.setText(editEvent.getEditMessage());
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
		
	}

	@Override
	public void mouseEntered(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mouseExited(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mousePressed(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mouseReleased(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
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
		workbench.setSignature(trale.getSignature("signature"));
		//add a few feature structures for testing purposes
		workbench.storeStructure("complex", "!newdata\"Edge\"(S1(0\"word\")(V2\"phon\"(L3(S5(4\"cruel\"))))(V6\"qstore\"(S8(7\"list\")))(V9\"synsem\"(S11(10\"synsem\")(V12\"loc\"(S14(13\"loc\")(V15\"cat\"(S17(16\"cat\")(V18\"determined\"(S20(19\"boolean\")))(V21\"head\"(S23(22\"adj\")(V24\"mod\"(S26(25\"synsem\")(V27\"loc\"(S29(28\"loc\")(V30\"cat\"(S32(31\"cat\")(V33\"determined\"(S35(34\"minus\")))(V36\"head\"(S38(37\"noun\")(V39\"case\"(S41(40\"case\")))(V42\"mod\"(S44(43\"synsem_none\")))(V45\"pred\"(S47(46\"boolean\")))))(V48\"val\"(S50(49\"mgsat(val)\")))))(V51\"cont\"(S53(52\"nom_obj\")(V54\"index\"(#55 1))(V56\"restr\"(#57 2))))))(V58\"nonloc\"(S60(59\"mgsat(nonloc)\")))))(V61\"pred\"(S63(62\"minus\")))))(V64\"val\"(S66(65\"val\")(V67\"subj\"(L68))(V69\"comps\"(#70 0))))))(V71\"cont\"(S73(72\"nom_obj\")(V74\"index\"(#75 1))(V76\"restr\"(L77(S79(78\"psoa\")(V80\"nucleus\"(S82(81\"adjmod\")(V83\"inst\"(#84 1))(V85\"relationname\"(S87(86\"bot\")))(V88\"soa_arg\"(S90(89\"mgsat(psoa)\")))))(V91\"quants\"(L92)))(Z93(#94 2))))))))(V95\"nonloc\"(S97(96\"mgsat(nonloc)\")))))(V98\"arg_st\"(#99 0)))(R100 2(S2(101\"list\")))(R102 0(L0))(R103 1(S1(104\"index\")(V105\"gen\"(S107(106\"gen\")))(V108\"num\"(S110(109\"num\")))(V111\"pers\"(S113(112\"pers\")))(V114\"sort\"(S116(115\"bot\")))))\n");
		workbench.storeStructure("cruel", "!newdata \"cruel\" (S1(0\"mgsat\"))(T2 \"head_subject:cruel\" 1)\n");
		FeatureWorkbenchView workbenchView = new FeatureWorkbenchView(control, trale);
		workbenchView.setTitle("Feature Workbench");
		workbenchView.display(workbench);
		KahinaWindowManager wManager = new KahinaWindowManager(control);
		KahinaWindow workbenchWindow = wManager.integrateInDefaultWindow(workbenchView);
		wManager.registerWindow(workbenchWindow);
		wManager.displayWindows();
		workbenchWindow.setSize(800, 500);
		workbenchWindow.setLocation(200, 200);
		workbenchWindow.setVisible(true);
	}
}
