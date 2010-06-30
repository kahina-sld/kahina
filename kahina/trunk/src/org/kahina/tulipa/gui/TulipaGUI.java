package org.kahina.tulipa.gui;

import java.awt.Color;
import java.awt.event.KeyEvent;

import org.kahina.core.KahinaStep;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.visual.dag.KahinaDAGView;
import org.kahina.tralesld.TraleSLDStepType;
import org.kahina.tulipa.TulipaInstance;
import org.kahina.tulipa.TulipaStepStatus;
import org.kahina.tulipa.visual.grammar.TulipaGrammarView;

public class TulipaGUI extends KahinaGUI
{
    private TulipaInstance instance;
    
    protected KahinaDAGView mainDAGView;
    protected TulipaGrammarView grammarView;
    
    public TulipaGUI(Class<? extends KahinaStep> stepType, TulipaInstance instance)
    {
        super(stepType, instance);
        this.instance = instance;      
        
        mainDAGView = new KahinaDAGView();
        mainDAGView.setTitle("Item Graph");
        views.add(mainDAGView);
        livingViews.add(mainDAGView); 
        varNameToView.put("dag", mainDAGView);

        mainDAGView.setStatusColorEncoding(TulipaStepStatus.PRODUCTIVE, Color.WHITE);
        mainDAGView.setStatusColorEncoding(TulipaStepStatus.UNPRODUCTIVE, new Color(183,50,50));
        mainDAGView.setStatusColorEncoding(TulipaStepStatus.PREVENTED_PRODUCTION, Color.RED);
        mainDAGView.setVerticalDistance(6);
        mainDAGView.setHorizontalDistance(2);
        
        grammarView = new TulipaGrammarView();
        grammarView.setTitle("RCG grammar");
        views.add(grammarView);
        livingViews.add(grammarView);
        varNameToView.put("grammar", grammarView);
        
        getControlPanel().addControlButtonGroup("Control");
        getControlPanel().addControlButton("creep.png", "creep", "(C)ontinue to next step", "Control", KeyEvent.VK_C);
        getControlPanel().addControlButton("roundskip.png", "auto-complete", "(A)uto-complete this step", "Control", KeyEvent.VK_A);
        getControlPanel().addControlButton("reject.png", "fail", "make this step (F)ail", "Control", KeyEvent.VK_F);
        getControlPanel().addControlButton("leap.png", "leap", "(L)eap to next breakpoint match",  "Control", KeyEvent.VK_L);
        getControlPanel().addControlButton("stop.png", "stop", "abort skip or leap (X)",  "Control", KeyEvent.VK_X);
        
        getControlPanel().addControlButtonGroup("History");
        getControlPanel().addControlButton("back.png", "backInHistory", "Back (Q)",  "History", KeyEvent.VK_Q);
        getControlPanel().addControlButton("forward.png", "forwardInHistory", "Forward (W)",  "History", KeyEvent.VK_W);
    }
    
    protected void displayMainViews()
    {
        super.displayMainViews();
        mainDAGView.display(instance.getState().getDAG());  
        grammarView.display(instance.getState().getGrammar()); 
    }
    
    public void prepare()
    {
        super.prepare();
        getWindowForVarName("dag").setSize(800, 500);
        getWindowForVarName("dag").setLocation(0, 150);
        getWindowForVarName("grammar").setSize(800, 100);
        getWindowForVarName("grammar").setLocation(0, 700);
    }
}
