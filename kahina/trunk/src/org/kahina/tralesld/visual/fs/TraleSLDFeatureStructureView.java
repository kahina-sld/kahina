package org.kahina.tralesld.visual.fs;

import org.kahina.core.visual.KahinaView;
import org.kahina.tralesld.data.fs.TraleSLDFeatureStructure;

public class TraleSLDFeatureStructureView extends KahinaView<TraleSLDFeatureStructure>
{
    public TraleSLDFeatureStructureViewPanel wrapInPanel()
    {
        TraleSLDFeatureStructureViewPanel panel = new TraleSLDFeatureStructureViewPanel();
        panel.setView(this);
        return panel;
    }

    public String getGrisuMessage()
    {
        return model.grisuMessage;
    }
}
