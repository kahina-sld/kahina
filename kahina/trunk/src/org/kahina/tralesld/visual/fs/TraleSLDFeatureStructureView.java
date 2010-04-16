package org.kahina.tralesld.visual.fs;

import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.tralesld.data.fs.TraleSLDFeatureStructure;

public class TraleSLDFeatureStructureView extends
		KahinaView<TraleSLDFeatureStructure>
{

	@Override
	public KahinaViewPanel<TraleSLDFeatureStructure> wrapInPanel()
	{
		KahinaViewPanel<TraleSLDFeatureStructure> panel = new TraleSLDFeatureStructureViewPanel(this);
		panel.setView(this);
		return panel;
	}
	
	public String getGrisuMessage()
	{
		return model.grisuMessage;
	}

}
