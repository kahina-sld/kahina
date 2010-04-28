package org.kahina.tralesld.bridge;

/**
 * this class is responsible for communicating with TRALE via Jasper
 * we create an instance of this class via Jasper, and it acts as the information broker
 * the bridge can invoke an instance of Kahina via the start() method
 */

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import org.kahina.core.KahinaRunner;
import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.util.PrologUtilities;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.lp.LogicProgrammingStepType;
import org.kahina.lp.bridge.LogicProgrammingBridge;
import org.kahina.tralesld.TraleSLDInstance;
import org.kahina.tralesld.TraleSLDStep;
import org.kahina.tralesld.control.event.TraleSLDBridgeEvent;
import org.kahina.tralesld.control.event.TraleSLDBridgeEventType;
import org.kahina.tralesld.data.chart.TraleSLDChartEdgeStatus;
import org.kahina.tralesld.data.fs.TraleSLDFeatureStructure;
import org.kahina.tralesld.data.fs.TraleSLDVariableBinding;

public class TraleSLDBridge extends LogicProgrammingBridge
{
	// TODO keep current step in memory until information about another one
	// comes in, then store
	
	TraleSLDInstance kahina;

	ArrayList<Integer> activeEdgeStack;

	HashSet<Integer> successfulEdges;

	StringBuilder grisuMessage;

	public static final boolean verbose = true;

	public TraleSLDBridge(TraleSLDInstance kahina, KahinaGUI gui)
	{
		super(kahina, gui);
		this.kahina = kahina;
		activeEdgeStack = new ArrayList<Integer>();
		successfulEdges = new HashSet<Integer>();
		grisuMessage = new StringBuilder();
	}

	public void initializeParseTrace(String parsedSentenceList)
	{
		try
		{
			if (verbose) System.err.println("TraleSLDBridgeinitializeParseTrace(\"" + parsedSentenceList + "\")");
			List<String> wordList = PrologUtilities.parsePrologStringList(parsedSentenceList);
            KahinaChart chart = kahina.getState().getChart();
            for (int i = 0; i < wordList.size(); i++)
            {
                chart.setSegmentCaption(i, wordList.get(i));
            }
			TraleSLDStep newStep = generateStep();
			newStep.setGoalDesc("init");
			newStep.setExternalID(0);
			stepIDConv.put(0, newStep.getID());
			newStep.store();
			KahinaRunner.processEvent(new TraleSLDBridgeEvent(TraleSLDBridgeEventType.INIT, newStep.getID(), wordList.toString()));
			currentID = newStep.getID();
			KahinaRunner.processEvent(new KahinaSelectionEvent(newStep.getID()));
		} 
        catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerRuleApplication(int extID, int left, int right, String ruleName)
	{
		try
		{
			if (verbose)
				System.err.println("TraleSLDBridge.registerRuleApplication(" + extID + "," + left + "," + right + ",\"" + ruleName + "\")");
			KahinaChart chart = kahina.getState().getChart();
			int newEdgeID = chart.addEdge(left, right, ruleName, TraleSLDChartEdgeStatus.ACTIVE);
			activeEdgeStack.add(0, newEdgeID);

			TraleSLDStep newStep = generateStep();
			newStep.setGoalDesc("rule(" + ruleName + ")");
			newStep.setExternalID(extID);
			stepIDConv.put(extID, newStep.getID());
			kahina.getState().linkEdgeToNode(newEdgeID, newStep.getID());
			newStep.store();

			// let TraleSLDTreeBehavior do the rest
			KahinaRunner.processEvent(new TraleSLDBridgeEvent(TraleSLDBridgeEventType.RULE_APP, newStep.getID(), ruleName));

			// the following two actions and the structures they operate on seem
			// to
			// be superfluous
			// edgeRegister.put(internalStepID, currentEdge);
			// lastEdge = currentEdge;
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerChartEdge(int number, int left, int right, String ruleName)
	{
		try
		{
			if (verbose)
				System.err.println("TraleSLDBridge.registerChartEdge(" + number + "," + left + "," + right + ",\"" + ruleName + "\")");
			kahina.getState().getChart().addEdge(left, right, ruleName, TraleSLDChartEdgeStatus.SUCCESSFUL);
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerEdgeDependency(int motherID, int daughterID)
	{
		try
		{
			if (verbose)
				System.err.println("TraleSLDBridge.registerEdgeDependency(" + motherID + "," + daughterID + ")");
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerMessageChunk(String chunk)
	{
		try
		{
			if (verbose)
				System.err.println("registerMessageChunk(\"" + chunk + "\")");
			grisuMessage.append(chunk);
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	/**
	 * Register message ends for local trees.
	 * @param extID
	 * @param type
	 */
	public void registerMessageEnd(int extID, String key)
	{
		try
		{
			if (verbose)
				System.err.println("registerMessageEnd(" + extID + ",\"" + key + "\"): " + grisuMessage);
			TraleSLDStep step = TraleSLDStep.get(stepIDConv.get(extID));
			TraleSLDFeatureStructure fs = new TraleSLDFeatureStructure(grisuMessage.toString());
			if ("start".equals(key))
			{
				step.startFeatStruct = fs;
			} else if ("end".equals(key))
			{
				step.endFeatStruct = fs;
			}
			step.store();
			grisuMessage = new StringBuilder();
		} 
        catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}
	
	public void registerMessageEnd(int extID, String key, String varName, String type)
	{
        try
        {
            registerMessageEnd(extID, key, varName, null, type);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            System.exit(1);
        }
	}
	
	/**
	 * Register message ends for variable bindings.
	 * @param extID
	 * @param varName
	 * @param tag
	 * @param type
	 */
	public void registerMessageEnd(int extID, String key, String varName, String tag, String type)
	{
		try
		{
			if (verbose)
			{
				System.err.println("registerMessageEnd(" + extID + ",\"" + varName + ",\"" + tag + ",\"" + type + "): " + grisuMessage);
			}
			TraleSLDStep step = TraleSLDStep.get(stepIDConv.get(extID));
			TraleSLDVariableBinding binding = new TraleSLDVariableBinding(varName, tag, type, grisuMessage.toString());
			if ("start".equals(key))
			{
				step.startBindings.add(binding);
			} else
			{
				step.endBindings.add(binding);
			}
			grisuMessage = new StringBuilder();
		}
		catch (Exception e)
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
			if (verbose)
				System.err.println("registerStepFailure(" + externalStepID + ")");
			super.registerStepFailure(externalStepID);
			int stepID = convertStepID(externalStepID);

			String command = LogicProgrammingStep.get(stepID).getGoalDesc();
			// need to handle bug: step failure is called even if edge was
			// successful
			if (command.startsWith("rule("))
			{
				int currentEdge = activeEdgeStack.remove(0);
				if (successfulEdges.contains(currentEdge))
				{
					System.err.println("Successful edge! Deleting from chart model...");
					kahina.getState().getChart().removeEdge(currentEdge);
					// TODO: was SUCCESS in the original; what exactly is the
					// difference?
					LogicProgrammingStep.get(stepID).setType(LogicProgrammingStepType.EXIT);
				}
				// current rule application failed; adapt chart accordingly
				else
				{
					System.err.println("Failed edge! Leaving it on the chart as junk...");
					kahina.getState().getChart().setEdgeStatus(currentEdge, TraleSLDChartEdgeStatus.FAILED);
					// TODO: devise a way of separating edge activation from
					// status
					// currentEdge.active = false;
					LogicProgrammingStep.get(stepID).setType(LogicProgrammingStepType.FAIL);
				}
				// move up one level in overview tree (really necessary?)
				// currentOverviewTreeNode =
				// tracer.overviewTraceView.treeNodes.get(currentOverviewTreeNode).getParent();
				// lastEdge = edgeRegister.getData(currentOverviewTreeNode);
			}
			currentID = stepID;
			KahinaRunner.processEvent(new KahinaSelectionEvent(stepID));
		} catch (Exception e)
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
				System.err.println("TraleSLDBridge.registerStepFinished(" + extID + ")");
			int stepID = convertStepID(extID);
			KahinaRunner.processEvent(new TraleSLDBridgeEvent(TraleSLDBridgeEventType.STEP_FINISHED, stepID));
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
		if (verbose) System.err.println("TraleSLDBridge.generateStep()");
		return new TraleSLDStep();
	}

}
