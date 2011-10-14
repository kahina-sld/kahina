package org.kahina.tralesld.visual.workbench;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;

import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.tralesld.TraleSLDState;
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
	private final JTable table;
	private final TraleSLDFeatureStructureEditor editor;
	private final TableModel tableModel = new DefaultTableModel();
	
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
		
		table = new JTable(tableModel);
		table.getSelectionModel().addListSelectionListener(this);
		JScrollPane tableScrollPane = new JScrollPane(table);
		table.setFillsViewportHeight(true);
		contentPanel.add(tableScrollPane);
		
		editor = new TraleSLDFeatureStructureEditor(state);
		table.getSelectionModel().addListSelectionListener(this);
		JScrollPane editorScrollPane = new JScrollPane(editor);
		contentPanel.add(editorScrollPane);
		
		add(contentPanel);
	}

	@Override
	public void updateDisplay() 
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void valueChanged(ListSelectionEvent arg0) 
	{
		// TODO Auto-generated method stub
		
	}

}
