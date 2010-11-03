package org.kahina.tulipa.bridge;

import java.util.HashMap;

import org.kahina.core.KahinaRunner;
import org.kahina.core.bridge.KahinaBridge;
import org.kahina.core.event.KahinaControlEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.tulipa.TulipaState;
import org.kahina.tulipa.TulipaStep;
import org.kahina.tulipa.event.TulipaBridgeEvent;
import org.kahina.tulipa.event.TulipaBridgeEventType;

public class TulipaBridge extends KahinaBridge
{
    public static final boolean verbose = false;
    
    //a dynamic map from external item IDs to latest corresponding dag nodes
    protected HashMap<Integer, Integer> itemIDConv;
    
    // store the state of the bridge, determining the next result of getNextCommand()
    protected char bridgeState = 'n';
    
    //in skip mode, this is the internal step ID of the step we are skipping
    int skipID = -1;
    
    // always contains the internal ID of the most recent step
    protected int currentID = -1;
    
    // always contains the internal ID of the selected step
    int selectedID;
    
    TulipaState state;
    
    int currentItem;
    boolean currentItemWasProductive = true;
    
    public TulipaBridge(TulipaState state)
    {
        super();
        this.state = state;
        bridgeState = 'n';
        itemIDConv = new HashMap<Integer, Integer>();
        currentItemWasProductive = false;
        currentItem = -1;
    }
    
    /**
     * convert external item IDs to internal IDs corresponding to dag nodes
     * uses entries in itemIDConv table, extending it together with the dag if
     * no entry was found
     * 
     * @return an internal step ID corresponding to the external ID
     */
    public int convertItemID(int extID)
    {
        if (verbose) System.err.println("TulipaBridge.convertItemID(" + extID + ")");
        Integer intID = itemIDConv.get(extID);
        if (intID == null)
        {
            TulipaStep newStep = generateStep();
            intID = state.nextStepID();
            newStep.setExternalID(extID);
            KahinaRunner.store(intID, newStep);
            itemIDConv.put(extID, intID);
        }
        if (verbose) System.err.println("TulipaBridge.convertItemID(" + extID + ") = " + intID);
        return intID;
    }
    
    @Override
	public TulipaStep generateStep()
    {
        if (verbose) System.err.println("TulipaBridge.generateStep()");
        return new TulipaStep();
    }
    
    public int getNextCommand()
    {
        switch (bridgeState)
        {
            case 'n':
            {
                return TulipaBridgeCommand.DO_NOTHING;
            }
            case 'c':
            {
                bridgeState = 'n';
                return TulipaBridgeCommand.CREEP;
            }
            case 'f':
            {
                bridgeState = 'n';
                return TulipaBridgeCommand.FAIL;
            }
            case 't':
            {
                if (verbose)
                {
                    System.err.println("Bridge state/pressed button: t/c");
                }
                bridgeState = 's';
                return 'c';
            }
            case 's':
            {
                if (skipID == currentID)
                {
                    if (verbose)
                    {
                        System.err.println("Bridge state/pressed button: s/n");
                    }
                    skipID = -1;
                    bridgeState = 'n';
                    KahinaRunner.processEvent(new KahinaSelectionEvent(currentID));
                    return 'n';
                } else
                {
                    if (verbose)
                    {
                        System.err.println("Bridge state/pressed button: s/c");
                    }
                    return 'c';
                }
            }
            case 'l':
            {
                return TulipaBridgeCommand.CREEP;
            }
        }
        return TulipaBridgeCommand.DO_NOTHING;
    }
    
    public void registerGrammarClause(int clauseID, String rcgClause)
    {
    	if (verbose) System.err.println("TulipaBridge.registerGrammarClause(" + clauseID + ",\"" + rcgClause + "\")");
    	state.getGrammar().addClause(clauseID, rcgClause);
    }
    
    public void initializeParse()
    {
        try
        {
            if (verbose) System.err.println("TulipaBridge.initializeParse()");
            KahinaRunner.processEvent(new TulipaBridgeEvent(TulipaBridgeEventType.INIT));
            if (verbose) System.err.println("//TulipaBridge.initializeParse()");
        } 
        catch (Exception e)
        {
            e.printStackTrace();
            System.exit(1);
        }
    }
    
    public void announceItemProcessing(int extID)
    {
        int stepID = convertItemID(extID);
        if (!currentItemWasProductive)
        {
            KahinaRunner.processEvent(new TulipaBridgeEvent(TulipaBridgeEventType.UNPRODUCTIVE, currentItem));
        }
        currentItem = stepID;
        currentItemWasProductive = false;
    }
    
    public void registerInitialItem(int newID)
    {
        try
        {
            if (verbose) System.err.println("TulipaBridge.registerInitialItem(" + newID + "\")");
            int newStepID = convertItemID(newID);
            KahinaRunner.processEvent(new TulipaBridgeEvent(TulipaBridgeEventType.START, newStepID));
            currentID = newStepID;
            if (bridgeState == 'n') KahinaRunner.processEvent(new KahinaSelectionEvent(newStepID));
            if (verbose) System.err.println("//TulipaBridge.registerInitialItem(" + newID + "\")");
        } 
        catch (Exception e)
        {
            e.printStackTrace();
            System.exit(1);
        }
    }
    
    public void registerScanEpsilonStep(int ancID, int newID)
    {
        try
        {
            if (verbose) System.err.println("TulipaBridge.registerScanEpsilon(" + ancID + ",\"" + newID + "\")");
            int ancStepID = convertItemID(ancID);
            int newStepID = convertItemID(newID);
            KahinaRunner.processEvent(new TulipaBridgeEvent(TulipaBridgeEventType.SCAN_EPSILON, newStepID, ancStepID));
            currentID = newStepID;
            currentItemWasProductive = true;
            if (bridgeState == 'n') KahinaRunner.processEvent(new KahinaSelectionEvent(newStepID));
            if (verbose) System.err.println("//TulipaBridge.registerScanEpsilon(" + ancID + ",\"" + newID + "\")");
        } 
        catch (Exception e)
        {
            e.printStackTrace();
            System.exit(1);
        }
    }
    
    public void registerScanStep(int ancID, int newID)
    {
        try
        {
            if (verbose) System.err.println("TulipaBridge.registerScan(" + ancID + ",\"" + newID + "\")");
            int ancStepID = convertItemID(ancID);
            int newStepID = convertItemID(newID);
            KahinaRunner.processEvent(new TulipaBridgeEvent(TulipaBridgeEventType.SCAN, newStepID, ancStepID));
            currentID = newStepID;
            currentItemWasProductive = true;
            if (bridgeState == 'n') KahinaRunner.processEvent(new KahinaSelectionEvent(newStepID));
            if (verbose) System.err.println("//TulipaBridge.registerScan(" + ancID + ",\"" + newID + "\")");
        } 
        catch (Exception e)
        {
            e.printStackTrace();
            System.exit(1);
        }
    }
    
    public void registerPredictStep(int ancID, int newID)
    {
        try
        {
            if (verbose) System.err.println("TulipaBridge.registerPredict(" + ancID + ",\"" + newID + "\")");
            int ancStepID = convertItemID(ancID);
            int newStepID = convertItemID(newID);
            KahinaRunner.processEvent(new TulipaBridgeEvent(TulipaBridgeEventType.PREDICT, newStepID, ancStepID));
            currentID = newStepID;
            currentItemWasProductive = true;
            if (bridgeState == 'n') KahinaRunner.processEvent(new KahinaSelectionEvent(newStepID));
            if (verbose) System.err.println("//TulipaBridge.registerPredict(" + ancID + ",\"" + newID + "\")");
        } 
        catch (Exception e)
        {
            e.printStackTrace();
            System.exit(1);
        }
    }
    
    public void registerSuspendStep(int anc1ID, int anc2ID, int newID)
    {
        try
        {
            if (verbose) System.err.println("TulipaBridge.registerSuspend(" + anc1ID + ",\"" + anc2ID + ",\"" + newID + "\")");
            int anc1StepID = convertItemID(anc1ID);
            int anc2StepID = convertItemID(anc2ID);
            int newStepID = convertItemID(newID);
            KahinaRunner.processEvent(new TulipaBridgeEvent(TulipaBridgeEventType.SUSPEND, newStepID, anc1StepID, anc2StepID));
            currentID = newStepID;
            currentItemWasProductive = true;
            if (bridgeState == 'n') KahinaRunner.processEvent(new KahinaSelectionEvent(newStepID));
            if (verbose) System.err.println("//TulipaBridge.registerSuspend(" + anc1ID + ",\"" + anc2ID + ",\"" + newID + "\")");
        } 
        catch (Exception e)
        {
            e.printStackTrace();
            System.exit(1);
        }
    }
    
    public void registerResumeStep(int anc1ID, int anc2ID, int newID)
    {
        try
        {
            if (verbose) System.err.println("TulipaBridge.registerResume(" + anc1ID + ",\"" + anc2ID + ",\"" + newID + "\")");
            int anc1StepID = convertItemID(anc1ID);
            int anc2StepID = convertItemID(anc2ID);
            int newStepID = convertItemID(newID);
            KahinaRunner.processEvent(new TulipaBridgeEvent(TulipaBridgeEventType.RESUME, newStepID, anc1StepID, anc2StepID));
            currentID = newStepID;
            currentItemWasProductive = true;
            if (bridgeState == 'n') KahinaRunner.processEvent(new KahinaSelectionEvent(newStepID));
            if (verbose) System.err.println("//TulipaBridge.registerResume(" + anc1ID + ",\"" + anc2ID + ",\"" + newID + "\")");
        } 
        catch (Exception e)
        {
            e.printStackTrace();
            System.exit(1);
        }
    }
    
    public void registerItemLabel(int extID, String label)
    {
        try
        {
            if (verbose) System.err.println("TulipaBridge.registerItemLabel(" + extID + ",\"" + label + "\")");
            int stepID = convertItemID(extID);
            TulipaStep step = TulipaStep.get(stepID);
            step.setItemDesc(label.substring(0, label.indexOf("-->")));
            KahinaRunner.store(stepID, step);
            KahinaRunner.processEvent(new TulipaBridgeEvent(TulipaBridgeEventType.SET_ITEM_DESC, stepID, label.substring(0, label.indexOf("-->"))));
            currentID = stepID;
            if (verbose) System.err.println("//TulipaBridge.registerItemLabel(" + extID + ",\"" + label + "\")");
        } 
        catch (Exception e)
        {
            e.printStackTrace();
            System.exit(1);
        }
    }
    
    @Override
	protected void processControlEvent(KahinaControlEvent e)
    {
        String command = e.getCommand();
        if (command.equals("creep"))
        {
            if (bridgeState == 'n')
            {
                bridgeState = 'c';
            } 
            else if (bridgeState == 'p')
            {
                skipID = -1;
                bridgeState = 'c';
            } 
            else if (bridgeState == 'q')
            {
                skipID = -1;
                bridgeState = 'c';
            } 
            else if (bridgeState == 'l')
            {
                skipID = -1;
                bridgeState = 'n';
            }
        } 
        else if (command.equals("stop"))
        {
            if (bridgeState == 'p')
            {
                skipID = -1;
                bridgeState = 'c';
            } 
            else if (bridgeState == 'q')
            {
                skipID = -1;
                bridgeState = 'c';
            } 
            else if (bridgeState == 'l')
            {
                skipID = -1;
                bridgeState = 'n';
            }
        } 
        else if (command.equals("fail"))
        {
            if (bridgeState == 'n')
            {
                bridgeState = 'f';
            } else if (bridgeState == 'p')
            {
                skipID = -1;
                bridgeState = 'f';
            } else if (bridgeState == 'q')
            {
                skipID = -1;
                bridgeState = 'f';
            }
        } 
        else if (command.equals("auto-complete"))
        {
            if (bridgeState == 'n')
            {
                bridgeState = 't';
                if (selectedID == -1)
                {
                    skipID = currentID;
                } 
                else
                {
                    skipID = selectedID;
                }
            } 
            else if (bridgeState == 'p')
            {
                bridgeState = 't';
            } 
            else if (bridgeState == 'q')
            {
                bridgeState = 't';
                skipID = currentID;
            }        
        } 
        else if (command.equals("leap"))
        {
            if (bridgeState == 'n')
            {
                bridgeState = 'l';
            } else if (bridgeState == 'p')
            {
                bridgeState = 'l';
                skipID = -1;
            } else if (bridgeState == 'q')
            {
                bridgeState = 'l';
                skipID = -1;
            }
        } else if (command.equals("(un)pause"))
        {
            if (bridgeState == 't')
            {
                bridgeState = 'p';
            } else if (bridgeState == 's')
            {
                bridgeState = 'q';
            } else if (bridgeState == 'p')
            {
                bridgeState = 't';
            } else if (bridgeState == 'q')
            {
                bridgeState = 's';
            }
        }
    }
}
