package org.kahina.tralesld.bridge;

/**
 * this class is responsible for communicating with TRALE via Jasper
 * we create an instance of this class via Jasper, and it acts as the information broker
 * the bridge can invoke an instance of Kahina via the start() method
 */

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kahina.core.KahinaRunner;
import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.gui.event.KahinaChartUpdateEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.util.PrologUtilities;
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
	public static final boolean verbose = false;

	TraleSLDState state;

	List<Integer> prospectiveEdgeStack;

	private boolean prospectiveEdgeCanFail = false;

	Map<Integer, Integer> edgeIDConv;

	int lastRegisteredChartEdge = -1;

	public TraleSLDBridge(TraleSLDState state)
	{
		super(state);
		this.state = state;
		prospectiveEdgeStack = new ArrayList<Integer>();
		edgeIDConv = new HashMap<Integer, Integer>();
	}

	public void initializeParseTrace(String parsedSentenceList)
	{
		try
		{
			if (verbose)
				System.err.println("TraleSLDBridgeinitializeParseTrace(\"" + parsedSentenceList + "\")");
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
			// if (bridgeState == 'n') KahinaRunner.processEvent(new
			// KahinaSelectionEvent(newStep.getID()));
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	@Override
	public void registerStepInformation(int extID, String nodeLabel, String consoleMessage)
	{
		try
		{
			if (verbose)
			{
				System.err.println(this + ".registerStepInformation(" + extID + "," + nodeLabel + "," + consoleMessage + ")");
			}
			super.registerStepInformation(extID, nodeLabel, consoleMessage);
			if (nodeLabel.startsWith("rule_close") && lastRegisteredChartEdge != -1)
			{
				state.linkEdgeToNode(lastRegisteredChartEdge, currentID);
			}
			if (verbose)
			{
				System.err.println("//" + this + ".registerStepInformation(" + extID + "," + nodeLabel + "," + consoleMessage + ")");
			}
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	/**
	 * Called by {@link #registerRuleApplication(int, int, int, String, String)}
	 * to register the first prospective edge of a rule application, and
	 * directly via the Jasper interface to register any subsequent prospective
	 * edge of that rule application.
	 * 
	 * @param left
	 * @param right
	 * @param ruleApplicationExtID
	 * @param ruleName
	 */
	public void registerProspectiveEdge(int ruleApplicationExtID, String ruleName, int left, int right)
	{
		try
		{
			if (verbose)
			{
				System.err.println(this + ".registerProspectiveEdge(" + ruleApplicationExtID + "," + ruleName + "," + left + "," + right);
			}
			KahinaChart chart = state.getChart();
			int newEdgeID = chart.addEdge(left, right, ruleName, TraleSLDChartEdgeStatus.PROSPECTIVE);
			prospectiveEdgeStack.add(0, newEdgeID);
			prospectiveEdgeCanFail = true;
			state.linkEdgeToNode(newEdgeID, stepIDConv.get(ruleApplicationExtID));
			KahinaRunner.processEvent(new KahinaChartUpdateEvent(newEdgeID));
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerEdgeRetrieval(int daughterID)
	{
		try
		{
			if (verbose)
			{
				System.err.println(this + "registerEdgeRetrieval(" + daughterID + ")");
			}
			int daughter = edgeIDConv.get(daughterID);
			int mother = prospectiveEdgeStack.get(0);
			KahinaChart chart = state.getChart();
			chart.addEdgeDependency(mother, daughter);
			chart.setRightBoundForEdge(mother, chart.getRightBoundForEdge(mother) + chart.getRightBoundForEdge(daughter) - chart.getLeftBoundForEdge(daughter));
			//lastRegisteredChartEdge = mother;
			KahinaRunner.processEvent(new KahinaChartUpdateEvent(mother));
			if (verbose)
			{
				System.err.println("//" + this + ".registerEdgeRetrieval(" + daughterID + ")");
			}
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerRuleApplication(int extID, int left, int right, String ruleName, String consoleMessage, int leftmostDaughter)
	{
		try
		{
			if (verbose)
				System.err.println("TraleSLDBridge.registerRuleApplication(" + extID + "," + left + "," + right + ",\"" + ruleName + "\")");

			TraleSLDStep newStep = generateStep();
			newStep.setGoalDesc("rule(" + ruleName + ")");
			newStep.setExternalID(extID);
			stepIDConv.put(extID, newStep.getID());
			registerProspectiveEdge(extID, ruleName, left, right);
			newStep.storeCaching();

			// let TraleSLDTreeBehavior do the rest
			KahinaRunner.processEvent(new TraleSLDBridgeEvent(TraleSLDBridgeEventType.RULE_APP, newStep.getID(), ruleName));

			// experimental: message for console
			state.consoleMessage(newStep.getID(), extID, LogicProgrammingStepType.CALL, consoleMessage + " on [" + left + "," + right + "]");

			// if (bridgeState == 'n')
			{
				KahinaRunner.processEvent(new KahinaSelectionEvent(newStep.getID()));
			}
			// the following two actions and the structures they operate on seem
			// to
			// be superfluous
			// lastEdge = currentEdge;
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerChartEdge(int externalEdgeID, int left, int right, String ruleName)
	{
		try
		{
			if (verbose)
			{
				System.err.println("TraleSLDBridge.registerChartEdge(" + externalEdgeID + "," + left + "," + right + ",\"" + ruleName + "\")");
			}
			int internalEdgeID = state.getChart().addEdge(left, right, ruleName, TraleSLDChartEdgeStatus.SUCCESSFUL);
			if (verbose)
				System.err.println("Internal edge ID: " + internalEdgeID);
			edgeIDConv.put(externalEdgeID, internalEdgeID);
			lastRegisteredChartEdge = internalEdgeID;
			KahinaRunner.processEvent(new KahinaChartUpdateEvent(internalEdgeID));
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
			{
				System.err.println("TraleSLDBridge.registerEdgeDependency(" + motherID + "," + daughterID + ")");
			}
			state.getChart().addEdgeDependency(edgeIDConv.get(motherID), edgeIDConv.get(daughterID));
		} catch (Exception e)
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
		} catch (Exception e)
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
		} catch (Exception e)
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
			bridgeState = 'n';
			KahinaRunner.processEvent(new KahinaSelectionEvent(stepIDConv.get(0)));
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	@Override
	public void registerStepFailure(int externalStepID)
	{
		// TODO It would be better to have a TraleSLDChartBehavior listening to
		// KahinaBridge events. For example, we wouldn't have to fire two
		// selection events.
		try
		{
			if (verbose)
			{
				System.err.println("registerStepFailure(" + externalStepID + ")");
			}
			super.registerStepFailure(externalStepID);
			int stepID = convertStepID(externalStepID);
			if (prospectiveEdgeCanFail)
			{
				int currentEdge = prospectiveEdgeStack.remove(0);
				prospectiveEdgeCanFail = false;
				if (verbose)
				{
					System.err.println("Prospective edge " + currentEdge + " failed.");
				}
				state.getChart().setEdgeStatus(currentEdge, TraleSLDChartEdgeStatus.FAILED);
				state.linkEdgeToNode(currentEdge, stepID);
				//lastRegisteredChartEdge = currentEdge;
				KahinaRunner.processEvent(new KahinaChartUpdateEvent(currentEdge));
			}
			if (TraleSLDStep.get(stepID).getGoalDesc().startsWith("rule("))
			{
				prospectiveEdgeCanFail = true;
			}
			currentID = stepID;
			if (verbose)
			{
				System.err.println("Bridge state after chart edge was marked as failed: " + bridgeState);
			}
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

	@Override
	public TraleSLDStep generateStep()
	{
		// if (verbose) System.err.println("TraleSLDBridge.generateStep()");
		return new TraleSLDStep();
	}
}
