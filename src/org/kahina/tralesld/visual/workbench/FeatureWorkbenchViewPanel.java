package org.kahina.tralesld.visual.workbench;

import javax.swing.BoxLayout;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;

import org.kahina.core.visual.KahinaViewPanel;

public class FeatureWorkbenchViewPanel extends KahinaViewPanel<FeatureWorkbenchView> implements ListSelectionListener
{
	private final JTable table;
	private final TableModel tableModel = new DefaultTableModel();
	
	public FeatureWorkbenchViewPanel()
	{
		setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
		table = new JTable(tableModel);
		table.getSelectionModel().addListSelectionListener(this);
		JScrollPane tableScrollPane = new JScrollPane(table);
		table.setFillsViewportHeight(true);
		add(tableScrollPane);
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
