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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.kahina.core.KahinaRunner;
import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.event.KahinaControlEvent;
import org.kahina.core.gui.event.KahinaChartUpdateEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.util.PrologUtilities;
import org.kahina.core.util.Sharer;
import org.kahina.lp.LogicProgrammingStepType;
import org.kahina.lp.bridge.LogicProgrammingBridge;
import org.kahina.tralesld.TraleSLDState;
import org.kahina.tralesld.TraleSLDStep;
import org.kahina.tralesld.control.event.TraleSLDBridgeEvent;
import org.kahina.tralesld.control.event.TraleSLDBridgeEventType;
import org.kahina.tralesld.data.chart.TraleSLDChartEdgeStatus;
import org.kahina.tralesld.data.fs.TraleSLDFSPacker;
import org.kahina.tralesld.data.fs.TraleSLDVariableBinding;
import org.kahina.tralesld.event.TraleSLDControlEventCommands;

public class TraleSLDBridge extends LogicProgrammingBridge
{
	public static final boolean VERBOSE = false;

	private static final Pattern NOW_PATTERN = Pattern.compile("now\\((\\d+)\\)");

	TraleSLDState state;

	List<Integer> prospectiveEdgeStack;

	private boolean prospectiveEdgeCanFail = false;

	Map<Integer, Integer> edgeIDConv;

	int lastRegisteredChartEdge = -1;

	private TraleSLDFSPacker packer;

	private Sharer<TraleSLDVariableBinding> bindingSharer;

	public TraleSLDBridge(final TraleSLDState state)
	{
		super(state);
		this.state = state;
		prospectiveEdgeStack = new ArrayList<Integer>();
		edgeIDConv = new HashMap<Integer, Integer>();
		packer = new TraleSLDFSPacker();
		bindingSharer = new Sharer<TraleSLDVariableBinding>();
	}

	private void initializeChart(String parsedSentenceList)
	{
		List<String> wordList = PrologUtilities.parsePrologStringList(parsedSentenceList);
		KahinaChart chart = state.getChart();
		for (int i = 0; i < wordList.size(); i++)
		{
			chart.setSegmentCaption(i, wordList.get(i));
		}
	}

	public void step(int extID, String stepType, String nodeLabel, String consoleMessage)
	{
		try
		{
			if (VERBOSE)
			{
				System.err.println(this + ".step(" + extID + "," + stepType +  "," + nodeLabel + "," + consoleMessage + ")");
			}
			super.step(extID, nodeLabel, nodeLabel, consoleMessage);
			if (stepType.equals("rule_close") && lastRegisteredChartEdge != -1)
			{
				state.linkEdgeToNode(lastRegisteredChartEdge, currentID);
			} else if (stepType.equals("rec"))
			{
				if (VERBOSE)
				{
					System.err.println("Registering sentence...");
				}
				String sentence = nodeLabel.substring(3, nodeLabel.length());
				KahinaRunner.processEvent(new KahinaControlEvent(TraleSLDControlEventCommands.REGISTER_SENTENCE, new Object[] { PrologUtilities.parsePrologStringList(sentence) }));
				initializeChart(sentence);
			} else if (stepType.equals("compile_gram"))
			{
				if (VERBOSE)
				{
					System.err.println("Registering grammar...");
				}
				String absolutePath = PrologUtilities.atomLiteralToString(nodeLabel.substring(13, nodeLabel.length() - 1));
				KahinaRunner.processEvent(new KahinaControlEvent(TraleSLDControlEventCommands.REGISTER_GRAMMAR, new Object[] { absolutePath }));
			}
			if (VERBOSE)
			{
				System.err.println("Matching...");
			}
			Matcher matcher = NOW_PATTERN.matcher(nodeLabel);
			if (matcher.matches())
			{
				if (VERBOSE)
				{
					System.err.println("Matched! Current ID: " + currentID);
				}
				int blockingStepExtID = Integer.parseInt(matcher.group(1));
				int blockingStepID = stepIDConv.get(blockingStepExtID);
				linkNodes(currentID, blockingStepID);
			}
			if (VERBOSE)
			{
				System.err.println("Done matching.");
			}
			if (VERBOSE)
			{
				System.err.println("//" + this + ".step(" + extID + "," + stepType + "," + nodeLabel + "," + consoleMessage + ")");
			}
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	/**
	 * Called by {@link #registerRuleApplication(int, String, int, String)} to
	 * register the first prospective edge of a rule application, and directly
	 * via the Jasper interface to register any subsequent prospective edge of
	 * that rule application.
	 * 
	 * @param leftmostDaughter
	 *            pass {@code -1} to not register a leftmost daughter
	 */
	public void registerProspectiveEdge(int ruleApplicationExtID, String ruleName, int leftmostDaughter)
	{
		try
		{
			if (VERBOSE)
			{
				System.err.println(this + ".registerProspectiveEdge(" + ruleApplicationExtID + "," + ruleName + "," + leftmostDaughter);
			}
			KahinaChart chart = state.getChart();
			int newEdgeID = chart.addEdge(chart.getLeftBoundForEdge(edgeIDConv.get(leftmostDaughter)), chart.getRightBoundForEdge(edgeIDConv.get(leftmostDaughter)), ruleName,
					TraleSLDChartEdgeStatus.PROSPECTIVE);
			if (leftmostDaughter != -1)
			{
				chart.addEdgeDependency(newEdgeID, edgeIDConv.get(leftmostDaughter));
			}
			prospectiveEdgeStack.add(0, newEdgeID);
			prospectiveEdgeCanFail = true;
			state.linkEdgeToNode(newEdgeID, stepIDConv.get(ruleApplicationExtID));
			KahinaRunner.processEvent(new KahinaChartUpdateEvent(newEdgeID));
			if (VERBOSE)
			{
				System.err.println("//" + this + ".registerProspectiveEdge(" + ruleApplicationExtID + "," + ruleName + "," + leftmostDaughter);
			}
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
			if (VERBOSE)
			{
				System.err.println(this + "registerEdgeRetrieval(" + daughterID + ")");
			}
			int daughter = edgeIDConv.get(daughterID);
			int mother = prospectiveEdgeStack.get(0);
			KahinaChart chart = state.getChart();
			if (!chart.getDaughterEdgesForEdge(mother).contains(daughter))
			{
				if (VERBOSE)
				{
					System.err.println("Setting right bound for " + mother + " to that of" + daughter);
				}
				chart.setRightBoundForEdge(mother, chart.getRightBoundForEdge(daughter));
			}
			chart.addEdgeDependency(mother, daughter);
			// lastRegisteredChartEdge = mother;
			KahinaRunner.processEvent(new KahinaChartUpdateEvent(mother));
			if (VERBOSE)
			{
				System.err.println("//" + this + ".registerEdgeRetrieval(" + daughterID + ")");
			}
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerRuleApplication(int extID, String ruleName, int leftmostDaughter, String consoleMessage)
	{
		try
		{
			if (VERBOSE)
			{
				System.err.println("TraleSLDBridge.registerRuleApplication(" + extID + ",\"" + ruleName + "," + leftmostDaughter + "\")");
			}
			final TraleSLDStep newStep = generateStep();
			newStep.setGoalDesc("rule(" + ruleName + ")");
			newStep.setExternalID(extID);
			int newStepID = state.nextStepID();
			stepIDConv.put(extID, newStepID);
			registerProspectiveEdge(extID, ruleName, leftmostDaughter);
			if (VERBOSE)
			{
				System.err.println("Storing new step.");
			}
			KahinaRunner.store(newStepID, newStep);
			if (VERBOSE)
			{
				System.err.println("Firing rule application event.");
			}
			// let TraleSLDTreeBehavior do the rest
			KahinaRunner.processEvent(new TraleSLDBridgeEvent(TraleSLDBridgeEventType.RULE_APP, newStepID, ruleName, extID));
			if (VERBOSE)
			{
				System.err.println("Creating console message.");
			}
			// experimental: message for console
			state.consoleMessage(newStepID, extID, LogicProgrammingStepType.CALL, consoleMessage);
			if (VERBOSE)
			{
				System.err.println("Firing selection event.");
			}
			// if (bridgeState == 'n')
			{
				KahinaRunner.processEvent(new KahinaSelectionEvent(newStepID));
			}
			// the following two actions and the structures they operate on seem
			// to
			// be superfluous
			// lastEdge = currentEdge;
			if (VERBOSE)
			{
				System.err.println("//TraleSLDBridge.registerRuleApplication(" + extID + ",\"" + ruleName + "," + leftmostDaughter + "\")");
			}
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
			if (VERBOSE)
			{
				System.err.println("TraleSLDBridge.registerChartEdge(" + externalEdgeID + "," + left + "," + right + ",\"" + ruleName + "\")");
			}
			int internalEdgeID = state.getChart().addEdge(left, right, ruleName, TraleSLDChartEdgeStatus.SUCCESSFUL);
			if (VERBOSE)
				System.err.println("Internal edge ID: " + internalEdgeID);
			edgeIDConv.put(externalEdgeID, internalEdgeID);
			lastRegisteredChartEdge = internalEdgeID;
			KahinaRunner.processEvent(new KahinaChartUpdateEvent(internalEdgeID));
			if (VERBOSE)
			{
				System.err.println("//TraleSLDBridge.registerChartEdge(" + externalEdgeID + "," + left + "," + right + ",\"" + ruleName + "\")");
			}
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
			if (VERBOSE)
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

	public void registerMessage(int extID, String key, String grisuMessage)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".registerMessage(" + extID + "," + key + "," + grisuMessage);
		}
		try
		{
			int stepID = stepIDConv.get(extID);
			final TraleSLDStep step = TraleSLDStep.get(stepID);
			if ("start".equals(key))
			{
				step.startFeatStruct = packer.pack(grisuMessage);
			} else if ("end".equals(key))
			{
				step.endFeatStruct = packer.pack(grisuMessage);
			}
			KahinaRunner.store(stepID, step);
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerMessage(int extID, String key, String varName, String type, String grisuMessage)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".registerMessage(" + extID + "," + key + "," + varName + "," + grisuMessage);
		}
		try
		{
			registerMessage(extID, key, varName, null, type, grisuMessage);
			selectIfPaused(currentID);
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerMessage(int extID, String key, String varName, String tag, String type, String grisuMessage)
	{
		try
		{
			if (VERBOSE)
			{
				System.err.println(this + ".registerMessage(" + extID + "," + key + "," + varName + "," + tag + "," + type + "," + grisuMessage);
			}

			int id = stepIDConv.get(extID);
			TraleSLDStep step = TraleSLDStep.get(id);
			TraleSLDVariableBinding binding = bindingSharer.share(new TraleSLDVariableBinding(varName, tag, type, packer.pack(grisuMessage)));
			selectIfPaused(currentID);

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

	@Override
	public void fail(int externalStepID)
	{
		// TODO It would be better to have a TraleSLDChartBehavior listening to
		// KahinaBridge events. For example, we wouldn't have to fire two
		// selection events.
		try
		{
			if (VERBOSE)
			{
				System.err.println("registerStepFailure(" + externalStepID + ")");
			}
			super.fail(externalStepID);
			int stepID = convertStepID(externalStepID);
			if (prospectiveEdgeCanFail && !prospectiveEdgeStack.isEmpty())
			{
				int currentEdge = prospectiveEdgeStack.remove(0);
				prospectiveEdgeCanFail = false;
				if (VERBOSE)
				{
					System.err.println("Prospective edge " + currentEdge + " failed.");
				}
				state.getChart().setEdgeStatus(currentEdge, TraleSLDChartEdgeStatus.FAILED);
				state.linkEdgeToNode(currentEdge, stepID);
				// lastRegisteredChartEdge = currentEdge;
				KahinaRunner.processEvent(new KahinaChartUpdateEvent(currentEdge));
			}

			if (TraleSLDStep.get(stepID).getGoalDesc().startsWith("rule("))
			{
				prospectiveEdgeCanFail = true;
			}

			if (VERBOSE)
			{
				System.err.println("Bridge state after chart edge was marked as failed: " + bridgeState);
			}

			selectIfPaused(stepID);
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void finished(int extID)
	{
		try
		{
			if (VERBOSE)
			{
				System.err.println("LogicProgrammingBridge.registerStepFailure(" + extID + ")");
			}
			int stepID = convertStepID(extID);
			if (stepID == waitingForReturnFromSkip)
			{
				waitingForReturnFromSkip = -1;
			}
			KahinaRunner.processEvent(new TraleSLDBridgeEvent(TraleSLDBridgeEventType.STEP_FINISHED, stepID));
			currentID = stepID;
			parentCandidateID = state.getSecondaryStepTree().getParent(stepID);

			// TODO update a TraleSLDLineReference (that class doesn't exist
			// yet) or rewrite the whole thing – why are console messages line
			// references?

			// stop autocomplete/leap when we're done
			if (stepID == state.getStepTree().getRootID() && bridgeState != 'n')
			{
				KahinaRunner.processEvent(new KahinaSelectionEvent(stepID));
				bridgeState = 'c';
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

	@Override
	public TraleSLDStep generateStep()
	{
		// if (verbose) System.err.println("TraleSLDBridge.generateStep()");
		return new TraleSLDStep();
	}
}