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

import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.gui.event.KahinaChartUpdateEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.core.util.Sharer;
import org.kahina.lp.LogicProgrammingStepType;
import org.kahina.lp.bridge.LogicProgrammingBridge;
import org.kahina.prolog.util.PrologUtil;
import org.kahina.tralesld.TraleSLDInstance;
import org.kahina.tralesld.TraleSLDState;
import org.kahina.tralesld.TraleSLDStep;
import org.kahina.tralesld.control.TraleSLDControlEventCommands;
import org.kahina.tralesld.data.chart.TraleSLDChartEdgeStatus;
import org.kahina.tralesld.data.fs.TraleSLDFSPacker;
import org.kahina.tralesld.data.fs.TraleSLDVariableBinding;

public class TraleSLDBridge extends LogicProgrammingBridge
{
	public static final boolean VERBOSE = false;

	private static final Pattern NOW_PATTERN = Pattern.compile("now\\((\\d+)\\)");

	TraleSLDState state;

	List<Integer> prospectiveEdgeStack;

	private boolean prospectiveEdgeCanFail = false;

	Map<Integer, Integer> edgeIDConv;

	int lastRegisteredChartEdge = -1;

	private final TraleSLDFSPacker packer;

	private Sharer<TraleSLDVariableBinding> bindingSharer;

	public TraleSLDBridge(final TraleSLDInstance kahina)
	{
		super(kahina);
		this.state = kahina.getState();
		prospectiveEdgeStack = new ArrayList<Integer>();
		edgeIDConv = new HashMap<Integer, Integer>();
		packer = new TraleSLDFSPacker();
		bindingSharer = new Sharer<TraleSLDVariableBinding>();
	}

	private void initializeChart(String parsedSentenceList)
	{
		List<String> wordList = PrologUtil.parsePrologStringList(parsedSentenceList);
		KahinaChart chart = state.getChart();
		for (int i = 0; i < wordList.size(); i++)
		{
			chart.setSegmentCaption(i, wordList.get(i));
		}
	}
	
	public void registerSubtype(String type, String subtype)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".registerSubtype(" + type + "," + subtype +  ")");
		}
		state.getSignature().addSubtypeRelation(type, subtype);
	}
	
	public void registerAppropriateFeature(String type, String feature, String typeRest)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".registerAppropriateFeature(" + type + "," + feature + "," + typeRest + ")");
		}
		state.getSignature().addAppropriateFeature(type, feature, typeRest);
	}
	
	//the TraleSLDSignature can only infer all necessary information after
	//the entire type hierarchy with all the appropriateness conditions was registered
	public void signatureFinished()
	{
		if (VERBOSE)
		{
			System.err.println(this + ".signatureFinished()");
		}
		state.getSignature().inferCachedInformation();
		kahina.dispatchEvent(new KahinaControlEvent(TraleSLDControlEventCommands.REBUILD_SIGNATURE_INFO));
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
				kahina.dispatchEvent(new KahinaControlEvent(TraleSLDControlEventCommands.REGISTER_SENTENCE, new Object[] { PrologUtil.parsePrologStringList(sentence) }));
				initializeChart(sentence);
			} else if (stepType.equals("compile_gram"))
			{
				if (VERBOSE)
				{
					System.err.println("Registering grammar...");
				}
				String absolutePath = PrologUtil.atomLiteralToString(nodeLabel.substring(13, nodeLabel.length() - 1));
				kahina.dispatchEvent(new KahinaControlEvent(TraleSLDControlEventCommands.REGISTER_GRAMMAR, new Object[] { absolutePath }));
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
			kahina.dispatchEvent(new KahinaChartUpdateEvent(newEdgeID));
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
			kahina.dispatchEvent(new KahinaChartUpdateEvent(mother));
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
			state.store(newStepID, newStep);
			if (VERBOSE)
			{
				System.err.println("Firing rule application event.");
			}
			// let TraleSLDTreeBehavior do the rest
			kahina.dispatchEvent(new TraleSLDBridgeEvent(TraleSLDBridgeEventType.RULE_APP, newStepID, ruleName, extID));
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
				kahina.dispatchEvent(new KahinaSelectionEvent(newStepID));
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
			kahina.dispatchEvent(new KahinaChartUpdateEvent(internalEdgeID));
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
			final TraleSLDStep step = state.get(stepID);
			if ("start".equals(key))
			{
				step.startFeatStruct = packer.pack(grisuMessage);
			} else if ("end".equals(key))
			{
				step.endFeatStruct = packer.pack(grisuMessage);
			}
			state.store(stepID, step);
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
			TraleSLDStep step = state.get(id);
			TraleSLDVariableBinding binding = bindingSharer.share(new TraleSLDVariableBinding(varName, tag, type, packer.pack(grisuMessage)));

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
				kahina.dispatchEvent(new KahinaChartUpdateEvent(currentEdge));
			}

			if (state.get(stepID).getGoalDesc().startsWith("rule("))
			{
				prospectiveEdgeCanFail = true;
			}

			if (VERBOSE)
			{
				System.err.println("Bridge state after chart edge was marked as failed: " + getBridgeState());
			}

			// Stop autocomplete/leap when we're done. Also, set to creep so
			// we're sure to see the result and get the prompt back.
			if (isQueryRoot(stepID))
			{
				kahina.dispatchEvent(new KahinaSelectionEvent(stepID));
				setBridgeState('c');
			}

			maybeUpdateStepCount(false);
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
			kahina.dispatchEvent(new TraleSLDBridgeEvent(TraleSLDBridgeEventType.STEP_FINISHED, stepID));
			currentID = stepID;
			parentCandidateID = state.getSecondaryStepTree().getParent(stepID);

			// TODO update a TraleSLDLineReference (that class doesn't exist
			// yet) or rewrite the whole thing – why are console messages line
			// references?

			// Stop autocomplete/leap when we're done. Also, set to creep so
			// we're sure to see the result and get the prompt back.
			if (isQueryRoot(stepID))
			{
				kahina.dispatchEvent(new KahinaSelectionEvent(stepID));
				setBridgeState('c');
			}

			maybeUpdateStepCount(false);
			selectIfPaused(stepID);
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
