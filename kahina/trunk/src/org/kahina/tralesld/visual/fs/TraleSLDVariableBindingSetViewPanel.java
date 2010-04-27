package org.kahina.tralesld.visual.fs;

import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.kahina.core.visual.KahinaViewPanel;

public class TraleSLDVariableBindingSetViewPanel extends KahinaViewPanel<TraleSLDVariableBindingSetView> implements ListSelectionListener
{
	private static final long serialVersionUID = 8545282386910165013L;
	
	private static final boolean verbose = false;

	private final JTable table;
	
	private final JPanel innerPanel;

	private String shownVariable; // TODO preserve shown variable
	
	private final VariableBindingTableModel tableModel = new VariableBindingTableModel();;

	private VisualizationUtility util = VisualizationUtility.getDefault();

	public TraleSLDVariableBindingSetViewPanel()
	{
		if (verbose)
		{
			System.err.println("TraleSLDVariableBindingSetViewPanel()");
		}
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		table = new JTable(tableModel);
		table.getSelectionModel().addListSelectionListener(this);
		JScrollPane tableScrollPane = new JScrollPane(table);
		table.setFillsViewportHeight(true);
		add(tableScrollPane);
		innerPanel = new JPanel();
		add(new JScrollPane(innerPanel));
		if (verbose)
		{
			System.err.println("//TraleSLDVariableBindingSetViewPanel()");
		}
	}

	@Override
	public void updateDisplay()
	{
		if (verbose)
		{
			System.err.println("TraleSLDVariableBindingSetViewPanel.updateDisplay()");
		}
		innerPanel.removeAll();
		List<Integer> newSelectedRows = tableModel.setBindings(view.getModel(), table.getSelectedRows());
		ListSelectionModel selectionModel = table.getSelectionModel();
		for (int rowIndex : newSelectedRows)
		{
			selectionModel.addSelectionInterval(rowIndex, rowIndex);
		}
		if (verbose)
		{
			System.err.println("//TraleSLDVariableBindingSetViewPanel()");
		}
	}

	@Override
	public void valueChanged(ListSelectionEvent e)
	{
		innerPanel.removeAll();
		for (int i : table.getSelectedRows())
		{
			String varName = tableModel.getValueAt(i, 0);
			String grisuMessage = tableModel.getGrisuMessage(i);
			innerPanel.add(createFSFrame(varName, grisuMessage));
		}
		innerPanel.repaint();
	}

	private JPanel createFSFrame(String varName, String grisuMessage)
	{
		JPanel result = new JPanel();
		result.add(util.visualize(grisuMessage));
		result.setBorder(BorderFactory.createTitledBorder(varName));
		return result;
	}

}
