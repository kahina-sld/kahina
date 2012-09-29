package org.kahina.core.gui.breakpoint;

import java.awt.Component;
import java.awt.Dimension;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaState;
import org.kahina.core.control.KahinaController;
import org.kahina.core.edit.breakpoint.KahinaBreakpointEditorPanel;

public class ThresholdedBreakpointEditPanel extends KahinaBreakpointEditorPanel
{
	
	private static final long serialVersionUID = -5038545439301530295L;

	private JSpinner thresholdSpinner;
	
	private JLabel thresholdLabel1;
	
	private JLabel thresholdLabel2;
	
	private KahinaState state;

	public ThresholdedBreakpointEditPanel(KahinaInstance<?, ?, ?, ?> kahina)
	{
		super(kahina);
	}
	
	public void setState(KahinaState state)
	{
		this.state = state;
	}
	
	@Override
	protected void addAllComponents()
	{
		super.addAllComponents();
        add(Box.createRigidArea(new Dimension(0,5)));
		add(createThresholdPanel());
	}
	
	private Component createThresholdPanel()
	{
		JComponent thresholdPanel = new JPanel();
		thresholdPanel.setLayout(new BoxLayout(thresholdPanel, BoxLayout.LINE_AXIS));
		thresholdPanel.setMaximumSize(new Dimension(800, 30));
		thresholdPanel.add(createThresholdLabel1());
		thresholdPanel.add(createThresholdSpinner());
		thresholdPanel.add(createThresholdLabel2());
		thresholdPanel.add(Box.createGlue());
		return thresholdPanel;
	}
	
	private JLabel createThresholdLabel1()
	{
		thresholdLabel1 = new JLabel("Warn after ");
		return thresholdLabel1;
	}
	
	private JLabel createThresholdLabel2()
	{
		thresholdLabel2 = new JLabel(" matches");
		return thresholdLabel2;
	}

	private Component createThresholdSpinner()
	{
		thresholdSpinner = new JSpinner(new SpinnerNumberModel(1, 1, null, 1));
		return thresholdSpinner;
	}
	
	@Override
	protected void activateAllComponents()
	{
		super.activateAllComponents();
		thresholdLabel1.setEnabled(true);
		thresholdSpinner.setEnabled(true);
		thresholdLabel2.setEnabled(true);
	}
	
	@Override
	protected void deactivateAllComponents()
	{
		super.deactivateAllComponents();
		thresholdLabel1.setEnabled(false);
		thresholdSpinner.setEnabled(false);
		thresholdLabel2.setEnabled(false);
	}

	@Override
	public void showBreakpoint()
	{
		Integer threshold = state.getWarnThresholdByBreakpoint().get(breakpoint);
		if (threshold != null)
		{
			thresholdSpinner.setValue(threshold);
		}
		super.showBreakpoint();
	}

	public int getThreshold()
	{
		return (Integer) thresholdSpinner.getValue();
	}

}
