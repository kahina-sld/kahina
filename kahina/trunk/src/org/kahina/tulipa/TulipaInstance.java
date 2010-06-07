package org.kahina.tulipa;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.data.KahinaDataHandlingMethod;
import org.kahina.core.gui.KahinaViewRegistry;
import org.kahina.tralesld.TraleSLDState;
import org.kahina.tralesld.TraleSLDStep;
import org.kahina.tralesld.behavior.TraleSLDTreeBehavior;
import org.kahina.tralesld.bridge.TraleSLDBridge;
import org.kahina.tralesld.data.fs.TraleSLDFeatureStructure;
import org.kahina.tralesld.data.fs.TraleSLDVariableBindingSet;
import org.kahina.tralesld.gui.TraleSLDGUI;
import org.kahina.tralesld.visual.fs.TraleSLDFeatureStructureView;
import org.kahina.tralesld.visual.fs.TraleSLDVariableBindingSetView;
import org.kahina.tulipa.behavior.TulipaDAGBehavior;
import org.kahina.tulipa.bridge.TulipaBridge;
import org.kahina.tulipa.gui.TulipaGUI;

public class TulipaInstance extends KahinaInstance<TulipaState, TulipaGUI, TulipaBridge>
{
    public TulipaInstance()
    {
        // TODO: this reeks a wee bit of Bad Software Design
        new TulipaDAGBehavior(state.getDAG(), this);
        //gui = new TraleSLDGUI(TraleSLDStep.class, this);
        //bridge = new TraleSLDBridge(this, gui);
    }

    @Override
    protected TulipaBridge createBridge()
    {
        return new TulipaBridge(this.state);
    }

    @Override
    protected TulipaGUI createGUI()
    {
        return new TulipaGUI(TulipaStep.class, this);
    }

    @Override
    protected TulipaState createState()
    {

        if (KahinaRunner.getDatabaseHandler() != null)
        {
            return new TulipaState(this, KahinaDataHandlingMethod.DATABASE);
        } else
        {
            return new TulipaState(this, KahinaDataHandlingMethod.MEMORY);
        }
    }

    public TulipaState getState()
    {
        return state;
    }

    public TulipaBridge getBridge()
    {
        return bridge;
    }

    protected void fillViewRegistry()
    {
        super.fillViewRegistry();
        KahinaViewRegistry.registerMapping(TraleSLDFeatureStructure.class, TraleSLDFeatureStructureView.class);
        KahinaViewRegistry.registerMapping(TraleSLDVariableBindingSet.class, TraleSLDVariableBindingSetView.class);
    }
}
