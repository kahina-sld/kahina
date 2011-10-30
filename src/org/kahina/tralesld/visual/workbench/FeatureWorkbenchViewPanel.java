package org.kahina.tralesld.visual.workbench;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Enumeration;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JList;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.DefaultListModel;

import org.kahina.core.KahinaRunner;
import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.tralesld.TraleSLDState;
import org.kahina.tralesld.bridge.AuxiliaryTraleInstance;
import org.kahina.tralesld.data.FeatureWorkbench;
import org.kahina.tralesld.data.fs.TraleSLDFS;
import org.kahina.tralesld.data.fs.TraleSLDPackedFSTerminal;
import org.kahina.tralesld.visual.fs.TraleSLDFeatureStructureEditor;
import org.kahina.tralesld.visual.fs.TraleSLDFeatureStructureView;

/**
 * A feature workbench window, with list of objects on the left, 
 * a toolbar with functionality such as unification and diff, 
 * and an editing component on the right.
 * 
 * @author jdellert
 *
 */
public class FeatureWorkbenchViewPanel extends KahinaViewPanel<FeatureWorkbenchView> implements ListSelectionListener, ActionListener
{
	private final JLabel msgLabel;
	private final JLabel signatureFileLabel;
	private final JLabel theoryFileLabel;
	
	private JMenu newTypeInstanceMenu;
	
	private final JList list;
	private final TraleSLDFeatureStructureEditor editor;
	
	private AuxiliaryTraleInstance trale;
	
	public FeatureWorkbenchViewPanel(TraleSLDState state)
	{	
		this.trale = state.getTrale();
		
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		
		JMenuBar menuBar = new JMenuBar();
		menuBar.setMinimumSize(new Dimension(200,50));
		menuBar.setMaximumSize(new Dimension(500,50));
		
		JMenu workbenchMenu = new JMenu("Workbench");
		
		JMenuItem newWorkbenchItem = new JMenuItem("New Workbench");
		workbenchMenu.add(newWorkbenchItem);
		
		JMenuItem importWorkbenchItem = new JMenuItem("Import Workbench");
		workbenchMenu.add(importWorkbenchItem);
		
		JMenuItem exportWorkbenchItem = new JMenuItem("Export Workbench");
		workbenchMenu.add(exportWorkbenchItem);
		
		JMenuItem exportSelectionItem = new JMenuItem("Export Selection to Workbench");
		workbenchMenu.add(exportSelectionItem);
		
		menuBar.add(workbenchMenu);
		
		JMenu fsMenu = new JMenu("Feature Structure");
		
		newTypeInstanceMenu = new JMenu("New FS of type ...");
		fsMenu.add(newTypeInstanceMenu);
		
		JMenuItem fsClipboardItem = new JMenuItem("Paste FS from clipboard");
		fsMenu.add(fsClipboardItem);
		
		fsMenu.addSeparator();
		
		JMenuItem fsFileItem = new JMenuItem("Load FS from GRISU file");
		fsMenu.add(fsFileItem);
		
		JMenuItem exportFSItem = new JMenuItem("Save FS to GRISU file");
		fsMenu.add(exportFSItem);
		
		fsMenu.addSeparator();
		
		JMenuItem theoryMGSItem = new JMenuItem("Add MGS according to theory");
		fsMenu.add(theoryMGSItem);
		
		JMenuItem sigMGUItem = new JMenuItem("Add MGU (signature only)");
		fsMenu.add(sigMGUItem);
		
		JMenuItem theMGUItem = new JMenuItem("Add MGU according to theory");
		fsMenu.add(theMGUItem);
		
		menuBar.add(fsMenu);
		
		this.add(menuBar);
		
		JPanel controlPanel = new JPanel();
		controlPanel.setLayout(new BoxLayout(controlPanel, BoxLayout.Y_AXIS));
		
		signatureFileLabel = new JLabel("Signature file: ");
		controlPanel.add(signatureFileLabel);
		
		theoryFileLabel = new JLabel("Theory file: ");
		controlPanel.add(theoryFileLabel);
		
		msgLabel = new JLabel("Drag feature structures into this window.");
		controlPanel.add(msgLabel);
		
		add(controlPanel);
		
		JPanel contentPanel = new JPanel();
		contentPanel.setLayout(new BoxLayout(contentPanel, BoxLayout.X_AXIS));
		
		list = new JList();
		list.getSelectionModel().addListSelectionListener(this);
		JScrollPane listScroller = new JScrollPane(list);
		listScroller.setPreferredSize(new Dimension(250, 80));
		listScroller.setMaximumSize(new Dimension(300, 1000));
		listScroller.setAlignmentX(CENTER_ALIGNMENT);
		contentPanel.add(listScroller);
		
		editor = new TraleSLDFeatureStructureEditor(state);
		JScrollPane editorScrollPane = new JScrollPane(editor);
		contentPanel.add(editorScrollPane);
		
		add(contentPanel);
	}

	@Override
	public void updateDisplay() 
	{
		view.recalculate();
		list.setListData(view.getNameList().toArray());
		signatureFileLabel.setText("Signature file: " + view.getSignatureFileName());
		theoryFileLabel.setText("Theory file: " + view.getTheoryFileName());
		if (view.getModel().getSignature() != null)
		{
			for (String type : view.getModel().getSignature().getTypes())
			{
				JMenuItem typeItem = new JMenuItem(type);
				typeItem.addActionListener(this);
				newTypeInstanceMenu.add(typeItem);
			}
		}
		this.repaint();
	}

	@Override
	public void valueChanged(ListSelectionEvent arg0) 
	{
		String name = (String) list.getSelectedValue();;
		editor.loadGrisu(view.getModel().getStructure(name));	
		editor.updateDisplay();
	}
	
	public void actionPerformed(ActionEvent e)
	{
		String action = e.getActionCommand();
		//default: interpret action command as type ID
		String grisuString = trale.descToMgsGrisu(action);
		view.getModel().storeStructure("base: " + action, grisuString);
		updateDisplay();
	}
	
	private class FeatureStructureListModel extends DefaultListModel
	{	
		public int getSize()
		{
			return view.getNameList().size();
		}
		
		public Enumeration<String> elements()
		{
			return (Enumeration<String>) view.getNameList();
		}
		
		public String getElementAt(int i)
		{
			return view.getNameList().get(i);
		}
	}

}
