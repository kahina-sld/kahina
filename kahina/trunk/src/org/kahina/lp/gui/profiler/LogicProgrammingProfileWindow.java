package org.kahina.lp.gui.profiler;

import java.awt.Component;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.WindowConstants;

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
		JTable result = new JTable(profile.getTableModel());
		result.setAutoCreateRowSorter(true);
		result.setFillsViewportHeight(true);
		return result;
	}

}
