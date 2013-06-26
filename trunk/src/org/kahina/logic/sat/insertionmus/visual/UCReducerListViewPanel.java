package org.kahina.logic.sat.insertionmus.visual;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.GroupLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneLayout;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.dag.ColoredPathDAG;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.io.color.ColorUtil;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.muc.data.UCReducerList;
import org.kahina.logic.sat.muc.gui.WrapLayout;
import org.kahina.logic.sat.muc.heuristics.ReductionHeuristic;
import org.kahina.logic.sat.muc.task.ReductionAgent;

public class UCReducerListViewPanel extends KahinaView<UCReducerList>
{
	

	JPanel newReducerPanel;
    private JComboBox algorithmChooser;
    private JComboBox heuristicsChooser;
    protected JButton start;
    
    public UCReducerListViewPanel(KahinaInstance<?, ?, ?, ?> kahina) {
		super(kahina);
		// TODO Auto-generated constructor stub
		this.newReducerPanel = new JPanel();
		newReducerPanel.setLayout(new FlowLayout());
		algorithmChooser = new JComboBox();
		heuristicsChooser = new JComboBox();
		start = new JButton("Start");
		
		algorithmChooser.addItem("Default");
		algorithmChooser.addItem("Advanced only learn last");
		algorithmChooser.addItem("Advanced learn all");
		algorithmChooser.addItem("Binary search related algorithm");
		

		heuristicsChooser.addItem("Ascending");
		
		newReducerPanel.add(algorithmChooser);
		newReducerPanel.add(heuristicsChooser);
		newReducerPanel.add(start);
		
		
	}


	@Override
	public JComponent makePanel() {
		// TODO Auto-generated method stub
		return newReducerPanel;
	}
}
