package org.tralesld.bridge;

/**
 * this class is responsible for communicating with TRALE via Jasper
 * we create an instance of this class via Jasper, and it acts as the information broker
 * the bridge can invoke an instance of Kahina via the start() method
 */

import org.kahina.bridge.LogicProgrammingBridge;
import org.kahina.control.KahinaController;
import org.kahina.core.KahinaInstance;
import org.kahina.data.DbDataManager;
import org.kahina.gui.KahinaGUI;
import org.kahina.io.database.DatabaseHandler;

public class TraleSLDBridge extends LogicProgrammingBridge
{
	public void start()
	{
		System.err.print("Trying to build GUI window... ");
		try
		{
			// TODO Softcode choice between DbDataManager and MemDataManager
			// TODO database handler should be closed
			kahina = new KahinaInstance(
					new DbDataManager(new DatabaseHandler()));
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

	public void registerStepSourceCodeLocation(int id, String absolutePath,
			int lineNumber)
	{

	}

	public void registerRuleApplication(int id, int left, int right,
			String ruleName)
	{

	}

	public void registerChartEdge(int number, int left, int right,
			String ruleName)
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

	public void registerStepLocation(int externalStepID, int externalParentID)
	{

	}
	
	public void registerStepExit(int externalStepID)
	{
		
	}
	
	public void registerStepFailure(int externalStepID)
	{
		
	}
	
	public void registerStepFinished(int externalStepID)
	{
		
	}
	
	public void registerStepRedo(int externalStepID)
	{
		
	}

	public void registerBlockedPseudoStepInformation(int externalStepID,
			String goal)
	{

	}

	public void registerUnblockedPseudoStepInformation(int externalStepID,
			int externalBlockedPseudoStepID, String goal)
	{

	}

}
