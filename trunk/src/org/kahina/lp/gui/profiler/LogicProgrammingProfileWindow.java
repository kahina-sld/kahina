package org.kahina.lp.gui.profiler;

import java.awt.Component;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.RowSorter;
import javax.swing.SortOrder;
import javax.swing.WindowConstants;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;

import org.kahina.lp.profiler.LogicProgrammingProfile;

public class LogicProgrammingProfileWindow extends JFrame
{

	/**
	 * 
	 */
	private static final long serialVersionUID = 7993923998704688262L;
	
	public LogicProgrammingProfileWindow(LogicProgrammingProfile profile)
	{
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		setSize(800, 600);
		add(createMainPanel(profile));
	}

	private Component createMainPanel(LogicProgrammingProfile profile)
	{
		JPanel result = new JPanel();
		result.setLayout(new BoxLayout(result, BoxLayout.X_AXIS));
		result.add(new JScrollPane(createTable(profile)));
		return result;
	}

	private Component createTable(LogicProgrammingProfile profile)
	{
		TableModel model = profile.getTableModel();
		JTable result = new JTable(model);
		TableRowSorter<TableModel> sorter = new TableRowSorter<TableModel>(model);
		List<RowSorter.SortKey> keys = new ArrayList<RowSorter.SortKey>(2);
		keys.add(new RowSorter.SortKey(0, SortOrder.ASCENDING));
		keys.add(new RowSorter.SortKey(1, SortOrder.ASCENDING));
		sorter.setSortKeys(keys);
		result.setRowSorter(sorter);
		result.setFillsViewportHeight(true);
		return result;
	}

}
