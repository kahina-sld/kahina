package org.kahina.tralesld.data.tree;

import org.kahina.core.data.tree.DefaultLayerDecider;
import org.kahina.core.data.tree.KahinaTree;

public class TraleSLDLayerDecider extends DefaultLayerDecider
{
    //TODO: include decision logic for layering here (only two levels, as before)
    public int decideOnLayer(int nodeID, KahinaTree tree)
    {
        return 0;
    }
}
