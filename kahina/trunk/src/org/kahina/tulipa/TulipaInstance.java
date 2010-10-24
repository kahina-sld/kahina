package org.kahina.tulipa;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.gui.KahinaViewRegistry;
import org.kahina.tulipa.behavior.TulipaDAGBehavior;
import org.kahina.tulipa.bridge.TulipaBridge;
import org.kahina.tulipa.data.grammar.TulipaGrammar;
import org.kahina.tulipa.gui.TulipaGUI;
import org.kahina.tulipa.visual.grammar.TulipaGrammarView;

public class TulipaInstance extends KahinaInstance<TulipaState, TulipaGUI, TulipaBridge>
{
    public TulipaInstance()
    {
        //gui = new TraleSLDGUI(TraleSLDStep.class, this);
        //bridge = new TraleSLDBridge(this, gui);
    }
    
    @Override
    protected void createTreeBehavior()
    {
        new TulipaDAGBehavior(state.getDAG(), this);
    }

    @Override
    protected TulipaBridge createBridge()
    {
        return new TulipaBridge(this.state);
    }

    @Override
    protected TulipaGUI createGUI()
    {
        return new TulipaGUI(TulipaStep.class, this, KahinaRunner.getControl());
    }

    @Override
    protected TulipaState createState()
    {
    	return new TulipaState();
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
		KahinaViewRegistry.registerMapping(TulipaGrammar.class, TulipaGrammarView.class);
	}
}
