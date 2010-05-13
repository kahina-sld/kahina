package org.kahina.tralesld.bridge;

/**
 * this class is responsible for communicating with TRALE via Jasper
 * we create an instance of this class via Jasper, and it acts as the information broker
 * the bridge can invoke an instance of Kahina via the start() method
 */

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.kahina.core.KahinaRunner;
import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.gui.event.KahinaChartUpdateEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.util.PrologUtilities;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.lp.LogicProgrammingStepType;
import org.kahina.lp.bridge.LogicProgrammingBridge;
import org.kahina.tralesld.TraleSLDState;
import org.kahina.tralesld.TraleSLDStep;
import org.kahina.tralesld.control.event.TraleSLDBridgeEvent;
import org.kahina.tralesld.control.event.TraleSLDBridgeEventType;
import org.kahina.tralesld.data.chart.TraleSLDChartEdgeStatus;
import org.kahina.tralesld.data.fs.TraleSLDFeatureStructure;
import org.kahina.tralesld.data.fs.TraleSLDVariableBinding;

public class TraleSLDBridge extends LogicProgrammingBridge
{
    TraleSLDState state;
    
	List<Integer> activeEdgeStack;

	Set<Integer> successfulEdges;
	
	Map<Integer, Integer> edgeIDConv;

	public static final boolean verbose = false;

	public TraleSLDBridge(TraleSLDState state)
	{
		super(state);
        this.state = state;
		activeEdgeStack = new ArrayList<Integer>();
		successfulEdges = new HashSet<Integer>();
		edgeIDConv = new HashMap<Integer, Integer>();
	}

	public void initializeParseTrace(String parsedSentenceList)
	{
		try
		{
			if (verbose) System.err.println("TraleSLDBridgeinitializeParseTrace(\"" + parsedSentenceList + "\")");
			List<String> wordList = PrologUtilities.parsePrologStringList(parsedSentenceList);
			KahinaChart chart = state.getChart();
			for (int i = 0; i < wordList.size(); i++)
			{
				chart.setSegmentCaption(i, wordList.get(i));
			}
			TraleSLDStep newStep = generateStep();
			newStep.setGoalDesc("init");
			newStep.setExternalID(0);
			stepIDConv.put(0, newStep.getID());
			newStep.storeCaching();
			int id = newStep.getID();
			KahinaRunner.processEvent(new TraleSLDBridgeEvent(TraleSLDBridgeEventType.INIT, id, wordList.toString()));
			KahinaRunner.processEvent(new KahinaSelectionEvent(id));
			currentID = newStep.getID();
            
            state.consoleMessage(newStep.getID(), 0, LogicProgrammingStepType.CALL, "initialising parse: " + parsedSentenceList);
			//if (bridgeState == 'n') KahinaRunner.processEvent(new KahinaSelectionEvent(newStep.getID()));
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerRuleApplication(int extID, int left, int right, String ruleName, String consoleMessage)
	{
		try
		{
			if (verbose) System.err.println("TraleSLDBridge.registerRuleApplication(" + extID + "," + left + "," + right + ",\"" + ruleName + "\")");

			KahinaChart chart = state.getChart();
			int newEdgeID = chart.addEdge(left, right, ruleName, TraleSLDChartEdgeStatus.ACTIVE);
			activeEdgeStack.add(0, newEdgeID);

			TraleSLDStep newStep = generateStep();
			newStep.setGoalDesc("rule(" + ruleName + ")");
			newStep.setExternalID(extID);
			stepIDConv.put(extID, newStep.getID());
			state.linkEdgeToNode(newEdgeID, newStep.getID());
			newStep.storeCaching();

			// let TraleSLDTreeBehavior do the rest    
			KahinaRunner.processEvent(new TraleSLDBridgeEvent(TraleSLDBridgeEventType.RULE_APP, newStep.getID(), ruleName));
            
            //experimental: message for console
            state.consoleMessage(newStep.getID(), extID, LogicProgrammingStepType.CALL, consoleMessage+ " on [" + left + "," + right + "]");

            //if (bridgeState == 'n')
            {
                KahinaRunner.processEvent(new KahinaSelectionEvent(newStep.getID()));
            }
			// the following two actions and the structures they operate on seem
			// to
			// be superfluous
			// lastEdge = currentEdge;
		} 
        catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerChartEdge(int externalEdgeID, int left, int right, String ruleName)
	{
		try
		{
			if (verbose) System.err.println("TraleSLDBridge.registerChartEdge(" + externalEdgeID + "," + left + "," + right + ",\"" + ruleName + "\")");
			int internalEdgeID = state.getChart().addEdge(left, right, ruleName, TraleSLDChartEdgeStatus.SUCCESSFUL);
			if (verbose) System.err.println("Internal edge ID: " + internalEdgeID);
			edgeIDConv.put(externalEdgeID, internalEdgeID);
			KahinaRunner.processEvent(new KahinaChartUpdateEvent(internalEdgeID));
		} 
        catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerEdgeDependency(int motherID, int daughterID)
	{
		try
		{
			if (verbose) System.err.println("TraleSLDBridge.registerEdgeDependency(" + motherID + "," + daughterID + ")");
            state.getChart().addEdgeDependency(edgeIDConv.get(motherID), edgeIDConv.get(daughterID));
		}
        catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}
	
	public void registerActiveEdgeDependency(int daughterID)
	{
		try
		{
			if (verbose) System.err.println("TraleSLDBridge.registerActiveEdgeDependency(" + daughterID + ")");
            state.getChart().addEdgeDependency(activeEdgeStack.get(0), edgeIDConv.get(daughterID));
		} 
        catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	/**
	 * Register message ends for local trees.
	 * 
	 * @param extID
	 * @param type
	 */
	public void registerMessage(int extID, String key, String grisuMessage)
	{
		try
		{
			// if (verbose)
			// System.err.println("registerMessageEnd(" + extID + ",\"" + key +
			// "\"): " + grisuMessage);
			TraleSLDStep step = TraleSLDStep.get(stepIDConv.get(extID));
			TraleSLDFeatureStructure fs = new TraleSLDFeatureStructure(grisuMessage);
			if ("start".equals(key))
			{
				step.startFeatStruct = fs;
			} else if ("end".equals(key))
			{
				step.endFeatStruct = fs;
			}
			step.storeCaching();
		} 
        catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerMessage(int extID, String key, String varName, String type, String grisuMessage)
	{
		try
		{
			registerMessage(extID, key, varName, null, type, grisuMessage);
		} 
        catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	/**
	 * Register message ends for variable bindings.
	 * 
	 * @param extID
	 * @param varName
	 * @param tag
	 * @param type
	 */
	public void registerMessage(int extID, String key, String varName, String tag, String type, String grisuMessage)
	{
		try
		{
			// if (verbose)
			// {
			// System.err.println("registerMessageEnd(" + extID + ",\"" +
			// varName + ",\"" + tag + ",\"" + type + "): " + grisuMessage);
			// }
			TraleSLDStep step = TraleSLDStep.get(stepIDConv.get(extID));
			TraleSLDVariableBinding binding = new TraleSLDVariableBinding(varName, tag, type, grisuMessage);
			if ("start".equals(key))
			{
				step.startBindings.add(binding);
			} else
			{
				step.endBindings.add(binding);
			}
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerParseEnd()
	{
		try
		{
			if (verbose)
				System.err.println("registerParseEnd()");

		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerStepFailure(int externalStepID)
	{
		try
		{
			if (verbose) System.err.println("registerStepFailure(" + externalStepID + ")");
			super.registerStepFailure(externalStepID);
			int stepID = convertStepID(externalStepID);

			String command = LogicProgrammingStep.get(stepID).getGoalDesc();
			// need to handle bug: step failure is called even if edge was successful
			if (command.startsWith("rule("))
			{
				int currentEdge = activeEdgeStack.remove(0);
				if (successfulEdges.contains(currentEdge))
				{
					System.err.println("Successful edge! Deleting from chart model...");
					state.getChart().removeEdge(currentEdge);
				}
				// current rule application failed; adapt chart accordingly
				else
				{
					System.err.println("Failed edge! Leaving it on the chart as junk...");
					state.getChart().setEdgeStatus(currentEdge, TraleSLDChartEdgeStatus.FAILED);
				}
                
				// move up one level in overview tree (really necessary?)
				// currentOverviewTreeNode =
				// tracer.overviewTraceView.treeNodes.get(currentOverviewTreeNode).getParent();
				// lastEdge = edgeRegister.getData(currentOverviewTreeNode);
			}
			currentID = stepID;
            if (bridgeState == 'n')
            {
                KahinaRunner.processEvent(new KahinaSelectionEvent(stepID));
            }
		} 
        catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerStepFinished(int extID)
	{
		try
		{
			if (verbose)
			{
				System.err.println("LogicProgrammingBridge.registerStepFailure(" + extID + ")");
			}
			int stepID = convertStepID(extID);
			KahinaRunner.processEvent(new TraleSLDBridgeEvent(TraleSLDBridgeEventType.STEP_FINISHED, stepID));
			currentID = stepID;
			if (bridgeState == 'n') 
            {
				KahinaRunner.processEvent(new KahinaSelectionEvent(stepID));
			}
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerBlockedPseudoStepInformation(int extID, String goal)
	{
		try
		{
			if (verbose)
				System.err.println("registerBlockedPseudoStepInformation(" + extID + ",\"" + goal + "\")");
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerUnblockedPseudoStepInformation(int extID, int extBlockedPseudoStepID, String goal)
	{
		try
		{
			if (verbose)
				System.err.println("registerUnblockedPseudoStepInformation(" + extID + "," + extBlockedPseudoStepID + ",\"" + goal + "\")");
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public TraleSLDStep generateStep()
	{
		// if (verbose) System.err.println("TraleSLDBridge.generateStep()");
		return new TraleSLDStep();
	}
}
