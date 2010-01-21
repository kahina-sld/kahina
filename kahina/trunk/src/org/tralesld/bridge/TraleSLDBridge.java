package org.tralesld.bridge;

/**
 * this class is responsible for communicating with TRALE via Jasper
 * we create an instance of this class via Jasper, and it acts as the information broker
 * the bridge can invoke an instance of Kahina via the start() method
 */

import org.kahina.bridge.KahinaBridge;
import org.kahina.control.KahinaController;
import org.kahina.core.KahinaInstance;
import org.kahina.gui.KahinaGUI;

public class TraleSLDBridge extends KahinaBridge
{
    public void start()
    {
        System.err.print("Trying to build GUI window... ");
        try
        {
            kahina = new KahinaInstance();
            control = new KahinaController();
            gui = new KahinaGUI(kahina, control);
            System.err.println("Success.");
        } catch (Exception e)
        {
            e.printStackTrace();
        }
    }
    
    public void initializeParseTrace(String parsedSentenceList)
    {
        
    }
    
    public void registerStepInformation(int stepID, String command)
    {
        
    }
    
    public void registerStepSourceCodeLocation(int id, String absolutePath, int lineNumber)
    {
        
    }
    
    public void registerRuleApplication(int id, int left, int right, String ruleName)
    {
        
    }
    
    public void registerStepLocation(String callStack)
    {
        
    }
    
    public void registerStepRedo(String callStack)
    {
        
    }
    
    public void registerStepExit(String callStack, boolean deterministic)
    {
        
    }
    
    public void registerStepFinished(String callStack)
    {
        
    }
    
    public void registerStepFailure(String callStack)
    {
        
    }
    
    public void registerChartEdge(int number, int left, int right, String ruleName)
    {
        
    }
    
    public void registerEdgeDependency(int motherID, int daughterID)
    {
        
    }
    
    public void registerMessageChunk(String chunk)
    {
        
    }
    
    public void registerMessageEnd(int externalStepID, String type)
    {
        
    }
    
    public void registerParseEnd()
    {
        
    }
    
    public char getPressedButton()
    {
       return 'n';
    }
    
    
}
