package org.kahina.tralesld.visual.fs;

import gralej.parsers.ParseException;

import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.tralesld.data.fs.TraleSLDFeatureStructure;

public class TraleSLDFeatureStructureViewPanel extends
		KahinaViewPanel<TraleSLDFeatureStructureView>
{
	private TraleSLDFeatureStructureView v;
    
	private JPanel innerPanel;
	
	private VisualizationUtility util;

	private TraleSLDFeatureStructureView view;
	
	public TraleSLDFeatureStructureViewPanel()
	{
        v = new TraleSLDFeatureStructureView();
		util = VisualizationUtility.getDefault();
		innerPanel = new JPanel();
		innerPanel.setLayout(new BoxLayout(innerPanel, BoxLayout.Y_AXIS));
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		JScrollPane scrollPane = new JScrollPane(innerPanel);
		add(scrollPane);
	}
	
	@Override
	public void updateDisplay()
	{
		innerPanel.removeAll();
		try
		{
			innerPanel.add(util.visualize(view.getGrisuMessage()));
		} catch (ParseException e)
		{
			innerPanel.add(new JLabel("Parse error: " + e.getMessage()));
		}
		innerPanel.repaint();
	}

    @Override
    public void setView(TraleSLDFeatureStructureView view)
    {
        this.v = view;
    }

}
