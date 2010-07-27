package org.kahina.lp.bridge;

import java.util.HashMap;

import org.kahina.core.KahinaRunner;
import org.kahina.core.breakpoint.KahinaBreakpoint;
import org.kahina.core.bridge.KahinaBridge;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.event.KahinaControlEvent;
import org.kahina.core.event.KahinaEventTypes;
import org.kahina.core.event.KahinaSystemEvent;
import org.kahina.core.event.KahinaTreeEvent;
import org.kahina.core.event.KahinaTreeEventType;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.lp.LogicProgrammingStepType;
import org.kahina.lp.data.text.LogicProgrammingLineReference;
import org.kahina.lp.event.LogicProgrammingBridgeEvent;
import org.kahina.lp.event.LogicProgrammingBridgeEventType;

public class LogicProgrammingBridge extends KahinaBridge
{
	private static final boolean verbose = false;

	// a dynamic map from external step IDs to most recent corresponding tree
	// nodes
	protected HashMap<Integer, Integer> stepIDConv;

	// always contains the internal ID of the most recent step
	protected int currentID = -1;

	// always contains the interal ID of the selected step
	protected int selectedID = -1;

	// store the state of the bridge, determining the next result of
	// getPressedButton()
	protected char bridgeState = 'n';
	// used to hand on skip commands to the logic programming system
	protected boolean skipFlag = false;

	// in skip mode, this is the internal step ID of the step we are skipping
	int skipID = -1;

	LogicProgrammingState state;

	public LogicProgrammingBridge(LogicProgrammingState state)
	{
		super();
		this.state = state;
		stepIDConv = new HashMap<Integer, Integer>();
		KahinaRunner.getControl().registerListener(KahinaEventTypes.SYSTEM, this);
		KahinaRunner.getControl().registerListener(KahinaEventTypes.SELECTION, this);
		if (verbose)
			System.err.println("new LogicProgrammingBridge()");
	}

	/**
	 * convert external step IDs to internal IDs corresponding to tree nodes
	 * uses entries in stepIDConv table, extending it together with the tree if
	 * no entry was found
	 * 
	 * @return an internal step ID corresponding to the external ID
	 */
	public int convertStepID(int extID)
	{
		if (verbose)
			System.err.println("LogicProgrammingBridge.convertStepID(" + extID + ")");
		Integer intID = stepIDConv.get(extID);
		if (intID == null)
		{
			LogicProgrammingStep newStep = generateStep();
			intID = state.nextStepID();
			newStep.setExternalID(extID);
			KahinaRunner.store(intID, newStep);
			stepIDConv.put(extID, intID);
		}
		if (verbose)
			System.err.println("LogicProgrammingBridge.convertStepID(" + extID + ") = " + intID);
		return intID;
	}

	public void registerStepInformation(int extID, String nodeLabel, String consoleMessage)
	{
		try
		{
			if (verbose)
				System.err.println("LogicProgrammingBridge.registerStepInformation(" + extID + ",\"" + nodeLabel + "\")");
			int stepID = convertStepID(extID);
			LogicProgrammingStep step = LogicProgrammingStep.get(stepID);
			step.setGoalDesc(nodeLabel);
			step.setSourceCodeLocation(LogicProgrammingStep.get(currentID).getSourceCodeLocation());
			KahinaRunner.store(stepID, step);
			KahinaRunner.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.SET_GOAL_DESC, stepID, nodeLabel));
			currentID = stepID;

			state.consoleMessage(stepID, extID, LogicProgrammingStepType.CALL, consoleMessage);
			if (verbose)
				System.err.println("//LogicProgrammingBridge.registerStepInformation(" + extID + ",\"" + nodeLabel + "\")");
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerStepSourceCodeLocation(int extID, String absolutePath, int lineNumber)
	{
		try
		{
			if (verbose)
				System.err.println("LogicProgrammingBridge.registerStepSourceCodeLocation(" + extID + ",\"" + absolutePath + "\"," + lineNumber + ")");
			int stepID = convertStepID(extID);
			LogicProgrammingStep step = LogicProgrammingStep.get(stepID);
			step.setSourceCodeLocation(new KahinaSourceCodeLocation(absolutePath, lineNumber - 1, stepID));
			currentID = stepID;
			KahinaRunner.store(stepID, step);
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerStepLocation(int extID, int parentID)
	{
		try
		{
			if (verbose)
				System.err.println("LogicProgrammingBridge.registerStepLocation(" + extID + "," + parentID + ")");
			int stepID = convertStepID(extID);
			KahinaRunner.processEvent(new KahinaTreeEvent(KahinaTreeEventType.NEW_NODE, stepID, convertStepID(parentID)));
			currentID = stepID;
			if (verbose)
			{
				System.err.println("Bridge state: " + bridgeState);
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

	public void registerStepRedo(int extID)
	{
		try
		{
			if (verbose)
				System.err.println("LogicProgrammingBridge.registerStepRedo(" + extID + ")");
			int lastStepID = convertStepID(extID);
			LogicProgrammingStep lastStep = LogicProgrammingStep.get(lastStepID);
			LogicProgrammingStep newStep = lastStep.copy();
			int newStepID = state.nextStepID();
			KahinaRunner.store(newStepID, newStep);
			stepIDConv.put(extID, newStepID);
			KahinaRunner.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_REDO, lastStepID));
			currentID = newStepID;
			if (bridgeState == 'n')
				KahinaRunner.processEvent(new KahinaSelectionEvent(newStepID));

			LogicProgrammingLineReference ref = state.getConsoleLineRefForStep(lastStepID).generatePortVariant(LogicProgrammingStepType.REDO);
			ref.step = newStepID;
			// ref.store();
			state.consoleMessage(ref);
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerStepExit(int extID, boolean deterministic)
	{
		try
		{
			if (verbose)
				System.err.println("LogicProgrammingBridge.registerStepExit(" + extID + "," + deterministic + ")");
			int stepID = convertStepID(extID);
			if (deterministic)
			{
				KahinaRunner.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_DET_EXIT, stepID));
			} else
			{
				KahinaRunner.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_NONDET_EXIT, stepID));
			}
			currentID = stepID;
			if (bridgeState == 'n')
				KahinaRunner.processEvent(new KahinaSelectionEvent(stepID));

			LogicProgrammingLineReference ref = null;
			if (deterministic)
				ref = state.getConsoleLineRefForStep(stepID).generatePortVariant(LogicProgrammingStepType.DET_EXIT);
			else
			{
				ref = state.getConsoleLineRefForStep(stepID).generatePortVariant(LogicProgrammingStepType.EXIT);
			}
			// ref.store();
			state.consoleMessage(ref);
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerStepFailure(int extID)
	{
		try
		{
			if (verbose)
				System.err.println("LogicProgrammingBridge.registerStepFailure(" + extID + ")");
			int stepID = convertStepID(extID);
			KahinaRunner.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_FAIL, stepID));
			currentID = stepID;
			if (bridgeState == 'n')
				KahinaRunner.processEvent(new KahinaSelectionEvent(stepID));
			LogicProgrammingLineReference ref = state.getConsoleLineRefForStep(stepID).generatePortVariant(LogicProgrammingStepType.FAIL);
			// ref.store();
			state.consoleMessage(ref);
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public LogicProgrammingStep generateStep()
	{
		if (verbose)
			System.err.println("LogicProgrammingBridge.generateStep()");
		return new LogicProgrammingStep();
	}

	public char getPressedButton()
	{
		try
		{
			if (skipFlag)
			{
				if (verbose)
				{
					System.err.println("Bridge state/pressed button: " + bridgeState + "/s");
				}
				skipFlag = false;
				return 's';
			}
			switch (bridgeState)
			{
				case 'n':
				{
					if (verbose)
					{
						// System.err.println("Bridge state/pressed button: n/n");
					}
					return 'n';
				}
				case 'p':
				{
					if (verbose)
					{
						System.err.println("Bridge state/pressed button: p/n");
					}
					return 'n';
				}
				case 'q':
				{
					if (verbose)
					{
						System.err.println("Bridge state/pressed button: q/n");
					}
					return 'n';
				}
				case 'c':
				{
					if (verbose)
					{
						System.err.println("Bridge state/pressed button: c/c");
					}
					bridgeState = 'n';
					return 'c';
				}
				case 'f':
				{
					if (verbose)
					{
						System.err.println("Bridge state/pressed button: f/f");
					}
					bridgeState = 'n';
					return 'f';
				}
				case 'l':
				{
					if (verbose)
					{
						System.err.println("Bridge state/pressed button: l/c");
					}
					bridgeState = 'l';
					return 'c';
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
				case 'a':
				{
					if (verbose)
					{
						System.err.println("Bridge state/pressed button: a/a");
					}
					return 'a';
				}
				default:
				{
					if (verbose)
					{
						System.err.println("Bridge state/pressed button: " + bridgeState + "/n");
					}
					bridgeState = 'n';
					return 'n';
				}
			}
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
			throw new RuntimeException(); // dummy
		}
	}

	@Override
	protected void processSystemEvent(KahinaSystemEvent e)
	{
		if (e.getSystemEventType() == KahinaSystemEvent.QUIT)
		{
			bridgeState = 'a';
		}
	}

	@Override
	protected void processEvent(KahinaControlEvent e)
	{
		// TODO update chart when exiting leap/skip. Gah.
		String command = e.getCommand();
		if (command.equals("creep"))
		{
			if (bridgeState == 'n')
			{
				bridgeState = 'c';
			} else if (bridgeState == 'p')
			{
				skipID = -1;
				bridgeState = 'c';
			} else if (bridgeState == 'q')
			{
				skipID = -1;
				bridgeState = 'c';
			} else if (bridgeState == 'l')
			{
				skipID = -1;
				bridgeState = 'n';
			}
		} else if (command.equals("stop"))
		{
			if (bridgeState == 'p')
			{
				skipID = -1;
				bridgeState = 'c';
			} else if (bridgeState == 'q')
			{
				skipID = -1;
				bridgeState = 'c';
			} else if (bridgeState == 'l')
			{
				skipID = -1;
				bridgeState = 'n';
			}
		} else if (command.equals("fail"))
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
		} else if (command.equals("auto-complete"))
		{
			if (bridgeState == 'n')
			{
				bridgeState = 't';
				if (selectedID == -1)
				{
					skipID = currentID;
				} else
				{
					skipID = selectedID;
				}
			} else if (bridgeState == 'p')
			{
				bridgeState = 't';
			} else if (bridgeState == 'q')
			{
				bridgeState = 't';
				skipID = currentID;
			}
		} else if (command.equals("skip"))
		{
			skipFlag = true;
		} else if (command.equals("leap"))
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

	@Override
	protected void processEvent(KahinaSelectionEvent e)
	{
		selectedID = e.getSelectedStep();
		Integer linkTarget = state.getLinkTarget(selectedID);
		if (linkTarget != null)
		{
			// TODO only jump on doubleclick to reduce user surprise and risk
			// of event cycles
			KahinaRunner.processEvent(new KahinaSelectionEvent(linkTarget));
		}
	}

	protected void processSkipPointMatch(int nodeID, KahinaBreakpoint bp)
	{
		skipFlag = true;
	}

	protected void processCreepPointMatch(int nodeID, KahinaBreakpoint bp)
	{
		// no change if we are in leap or skip mode anyway
		if (bridgeState != 's' && bridgeState != 't' && bridgeState != 'l')
		{
			bridgeState = 'c';
		}
	}

	protected void processFailPointMatch(int nodeID, KahinaBreakpoint bp)
	{
		// TODO: handle this more elegantly if in skip or leap mode (possibly
		// additional state)
		bridgeState = 'f';
	}

	protected void processBreakPointMatch(int nodeID, KahinaBreakpoint bp)
	{
		// TODO: temporarily mark matching node in the breakpoint's signal color
		// same reaction as in pause mode
		if (bridgeState == 't')
		{
			bridgeState = 'p';
		} else if (bridgeState == 's')
		{
			bridgeState = 'q';
		} else if (bridgeState == 'l')
		{
			bridgeState = 'n';
		}
		state.breakpointConsoleMessage(currentID, "Breakpoint match: " + bp.getName() + " at node " + currentID);
	}
}
