package org.kahina.tralesld.visual.workbench;

import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JList;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.DefaultListModel;

import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.tralesld.TraleSLDState;
import org.kahina.tralesld.data.FeatureWorkbench;
import org.kahina.tralesld.data.fs.TraleSLDFS;
import org.kahina.tralesld.data.fs.TraleSLDPackedFSTerminal;
import org.kahina.tralesld.visual.fs.TraleSLDFeatureStructureEditor;

/**
 * A feature workbench window, with list of objects on the left, 
 * a toolbar with functionality such as unification and diff, 
 * and an editing component on the right.
 * 
 * @author jdellert
 *
 */
public class FeatureWorkbenchViewPanel extends KahinaViewPanel<FeatureWorkbenchView> implements ListSelectionListener
{
	private final JLabel msgLabel;
	private final JList list;
	private final TraleSLDFeatureStructureEditor editor;
	private final FeatureStructureListModel listModel;
	
	public FeatureWorkbenchViewPanel(TraleSLDState state)
	{
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		
		JPanel controlPanel = new JPanel();
		controlPanel.setLayout(new BoxLayout(controlPanel, BoxLayout.X_AXIS));
		
		String[] dummyEntries = { "lexicon", "entry1", "entry2", "entry3" };
		
		JComboBox lexBox = new JComboBox(dummyEntries);
		controlPanel.add(lexBox);
		
		JButton diffButton = new JButton("diff");
		controlPanel.add(diffButton);
		
		JButton mgsButton = new JButton("satisfy_the");
		controlPanel.add(mgsButton);
		
		JButton mguSigButton = new JButton("unify_sig");
		controlPanel.add(mguSigButton);
		
		JButton mguTheButton = new JButton("unify_the");
		controlPanel.add(mguTheButton);
		
		msgLabel = new JLabel("Drag feature structures into this window.");
		controlPanel.add(msgLabel);
		
		add(controlPanel);
		
		JPanel contentPanel = new JPanel();
		contentPanel.setLayout(new BoxLayout(contentPanel, BoxLayout.X_AXIS));
		
		listModel = new FeatureStructureListModel(this.view);
		list = new JList(listModel);
		list.getSelectionModel().addListSelectionListener(this);
		JScrollPane tableScrollPane = new JScrollPane(list);
		contentPanel.add(tableScrollPane);
		
		editor = new TraleSLDFeatureStructureEditor(state);
		JScrollPane editorScrollPane = new JScrollPane(editor);
		contentPanel.add(editorScrollPane);
		
		add(contentPanel);
	}
	
	//TODO: override display method to adapt list model

	@Override
	public void updateDisplay() 
	{

	}

	@Override
	public void valueChanged(ListSelectionEvent arg0) 
	{
		editor.view.display(listModel.getStructureAt(arg0.getFirstIndex()));	
	}
	
	private class FeatureStructureListModel extends DefaultListModel
	{
		FeatureWorkbenchView workbenchView;
		
		public FeatureStructureListModel(FeatureWorkbenchView workbench)
		{
			this.workbenchView = workbenchView;
		}
		
		public TraleSLDFS getStructureAt(int i)
		{
			return new TraleSLDPackedFSTerminal("ad_hoc_terminal");
		}
	}

}
