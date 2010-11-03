package org.kahina.lp.bridge;

import java.util.HashMap;
import java.util.Stack;

import org.kahina.core.KahinaException;
import org.kahina.core.KahinaRunner;
import org.kahina.core.breakpoint.KahinaBreakpoint;
import org.kahina.core.bridge.KahinaBridge;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.event.KahinaControlEvent;
import org.kahina.core.event.KahinaEventTypes;
import org.kahina.core.event.KahinaSystemEvent;
import org.kahina.core.event.KahinaTreeEvent;
import org.kahina.core.event.KahinaTreeEventType;
import org.kahina.core.event.KahinaWarnEvent;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.lp.LogicProgrammingStepType;
import org.kahina.lp.data.text.LogicProgrammingLineReference;
import org.kahina.lp.event.LogicProgrammingBridgeEvent;
import org.kahina.lp.event.LogicProgrammingBridgeEventType;

public class LogicProgrammingBridge extends KahinaBridge
{
	private static final boolean VERBOSE = false;

	// a dynamic map from external step IDs to most recent corresponding tree
	// nodes
	protected HashMap<Integer, Integer> stepIDConv;

	// always contains the internal ID of the most recent step
	protected int currentID = -1;

	// always contains the internal ID of the step which, if a call occurs, will
	// be the parent of the new step
	// TODO we can move this to the tree behavior so bridge doesn't have to
	// access the tree
	protected int parentCandidateID = -1;

	// always contains the internal ID of the selected step
	protected int selectedID = -1;

	// store the state of the bridge, determining the next result of
	// getPressedButton()
	protected char bridgeState = 'n';
	// used to hand on skip commands to the logic programming system
	protected boolean skipFlag = false;
	protected int waitingForReturnFromSkip = -1;

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
		if (VERBOSE)
			System.err.println("new LogicProgrammingBridge()");
	}

	/**
	 * convert external step IDs to internal IDs corresponding to tree nodes
	 * uses entries in stepIDConv table, extending it together with the tree if
	 * no entry was found
	 * 
	 * @return an internal step ID corresponding to the external ID
	 */
	protected int convertStepID(int extID)
	{
		if (VERBOSE)
			System.err.println("LogicProgrammingBridge.convertStepID(" + extID + ")");
		if (extID == -1)
		{
			return -1;
		}
		Integer intID = stepIDConv.get(extID);
		if (VERBOSE)
		{
			System.err.println("stepIDConv.get(" + extID + ")=" + intID);
		}
		if (intID == null)
		{
			LogicProgrammingStep newStep = generateStep();
			intID = state.nextStepID();
			newStep.setExternalID(extID);
			KahinaRunner.store(intID, newStep);
			stepIDConv.put(extID, intID);
		}
		if (VERBOSE)
			System.err.println("LogicProgrammingBridge.convertStepID(" + extID + ") = " + intID);
		return intID;
	}

	public void step(int extID, String nodeLabel)
	{
		try
		{
			if (VERBOSE)
				System.err.println("LogicProgrammingBridge.registerStepInformation(" + extID + ",\"" + nodeLabel + "\")");
			int stepID = convertStepID(extID);
			LogicProgrammingStep step = LogicProgrammingStep.get(stepID);
			step.setGoalDesc(nodeLabel);
			if (waitingForReturnFromSkip != -1)
			{
				state.hideStep(stepID);
			}
			if (currentID != -1)
			{
				step.setSourceCodeLocation(LogicProgrammingStep.get(currentID).getSourceCodeLocation());
			}
			KahinaRunner.store(stepID, step);
			KahinaRunner.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.SET_GOAL_DESC, stepID, nodeLabel));
			currentID = stepID;
			if (VERBOSE)
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
			if (VERBOSE)
				System.err.println("LogicProgrammingBridge.registerStepSourceCodeLocation(" + extID + ",\"" + absolutePath + "\"," + lineNumber + ")");
			int stepID = convertStepID(extID);
			LogicProgrammingStep step = LogicProgrammingStep.get(stepID);
			step.setSourceCodeLocation(new KahinaSourceCodeLocation(absolutePath, lineNumber - 1));
			currentID = stepID;
			KahinaRunner.store(stepID, step);
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void call(int extID)
	{
		try
		{
			if (VERBOSE)
				System.err.println("LogicProgrammingBridge.call(" + extID + ")");
			if (VERBOSE)
			{
				System.err.println("Converting step ID...");
			}
			int stepID = convertStepID(extID);
			if (VERBOSE)
			{
				System.err.println("Convertig parent ID...");
			}
			// used by tree behavior and profiler:
			KahinaRunner.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_CALL, stepID, parentCandidateID));
			// used by node counter:
			KahinaRunner.processEvent(new KahinaTreeEvent(KahinaTreeEventType.NEW_NODE, stepID, parentCandidateID));
			currentID = stepID;
			parentCandidateID = stepID;
			if (VERBOSE)
			{
				System.err.println("Bridge state: " + bridgeState);
			}
			selectIfPaused(stepID);
			if (VERBOSE)
				System.err.println("//LogicProgrammingBridge.call(" + extID + ")");
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void redo(int extID)
	{
		try
		{
			if (VERBOSE)
			{
				System.err.println("LogicProgrammingBridge.registerStepRedo(" + extID + ")");
			}

			int lastStepID = convertStepID(extID);
			int id = lastStepID;
			Stack<Integer> redoStack = new Stack<Integer>();
			KahinaTree callTree = state.getSecondaryStepTree();

			// Collect the steps we need to backtrack into, from the one being
			// redone up until (and excluding) the current parent candidate:
			do
			{
				if (VERBOSE)
				{
					System.err.println("Pushing " + id + " onto redo stack.");
				}
				
				redoStack.push(id);
				// Get the internal ID of the parent, or that of its copy if it already has one:
				id = stepIDConv.get(LogicProgrammingStep.get(callTree.getParent(id)).getExternalID());

				if (id == -1)
				{
					throw new KahinaException("Unexpected redo of " + lastStepID + " under " + parentCandidateID + ".");
				}
			} while (id != parentCandidateID);

			int newStepID = -1;

			// Create alternative copies of those steps to reflect backtracking,
			// working top-down:
			while (!redoStack.isEmpty())
			{
				id = redoStack.pop();
				LogicProgrammingStep lastStep = LogicProgrammingStep.get(id);
				LogicProgrammingStep newStep = lastStep.copy();
				newStep.incrementRedone();
				newStepID = state.nextStepID();
				KahinaRunner.store(newStepID, newStep);
				stepIDConv.put(lastStep.getExternalID(), newStepID);
				KahinaRunner.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_REDO, id));
			}

			currentID = newStepID;
			parentCandidateID = newStepID;

			selectIfPaused(newStepID);

			// TODO Do we also want console messages for each "virtual redo"? If
			// so, move this into loop above.
			LogicProgrammingLineReference reference = state.getConsoleLineRefForStep(lastStepID);
			if (reference != null)
			{
				state.consoleMessage(reference.generatePortVariant(LogicProgrammingStepType.REDO));
			}
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void exit(int extID, boolean deterministic)
	{
		try
		{
			if (VERBOSE)
				System.err.println("LogicProgrammingBridge.registerStepExit(" + extID + "," + deterministic + ")");
			int stepID = convertStepID(extID);
			if (stepID == waitingForReturnFromSkip)
			{
				waitingForReturnFromSkip = -1;
			}
			if (deterministic)
			{
				KahinaRunner.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_DET_EXIT, stepID));
			} else
			{
				KahinaRunner.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_NONDET_EXIT, stepID));
			}
			currentID = stepID;
			parentCandidateID = state.getSecondaryStepTree().getParent(stepID);
			
			// stop autocomplete/leap when we're done
			if (deterministic && stepID == state.getStepTree().getRootID() && bridgeState != 'n')
			{
				KahinaRunner.processEvent(new KahinaSelectionEvent(stepID));
				bridgeState = 'c';
			}
			
			selectIfPaused(stepID);
			LogicProgrammingLineReference reference = state.getConsoleLineRefForStep(stepID);
			if (reference != null)
			{
				if (deterministic)
				{
					state.consoleMessage(reference.generatePortVariant(LogicProgrammingStepType.DET_EXIT));
				} else
				{
					state.consoleMessage(reference.generatePortVariant(LogicProgrammingStepType.EXIT));
				}
			}
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void fail(int extID)
	{
		try
		{
			if (VERBOSE)
				System.err.println("LogicProgrammingBridge.registerStepFailure(" + extID + ")");
			int stepID = convertStepID(extID);
			if (stepID == waitingForReturnFromSkip)
			{
				waitingForReturnFromSkip = -1;
			}
			KahinaRunner.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_FAIL, stepID));
			currentID = stepID;
			parentCandidateID = state.getSecondaryStepTree().getParent(stepID);
			
			// stop autocomplete/leap when we're done
			if (stepID == state.getStepTree().getRootID() && bridgeState != 'n')
			{
				KahinaRunner.processEvent(new KahinaSelectionEvent(stepID));
				bridgeState = 'c';
			}
			
			selectIfPaused(stepID);
			LogicProgrammingLineReference reference = state.getConsoleLineRefForStep(stepID);
			if (reference != null)
			{
				state.consoleMessage(reference.generatePortVariant(LogicProgrammingStepType.FAIL));
			}
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}
	
	/**
	 * Selects the given step and updates the GUI if the debugger is not
	 * currently leaping or autocompleting.
	 */
	protected void selectIfPaused(int stepID)
	{
		if (bridgeState == 'n')
		{
			KahinaRunner.processEvent(new KahinaSelectionEvent(stepID));
		}
	}

	/**
	 * Call this to indicate that the top query succeeded or failed, providing
	 * the step ID of the top query. The bridge will then go back into creep
	 * mode.
	 */
	public void end(int extID)
	{
		try
		{
			bridgeState = 'n';
			KahinaRunner.processEvent(new KahinaSelectionEvent(currentID));
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	@Override
	protected LogicProgrammingStep generateStep()
	{
		if (VERBOSE)
			System.err.println("LogicProgrammingBridge.generateStep()");
		return new LogicProgrammingStep();
	}

	/**
	 * @return the action command for the tracer. Currently supported are:
	 *         {@code 'c'} for creep, {@code 's'} for skip, {@code 'f'} for
	 *         fail, {@code 'a'} for abort and {@code 'n'} if there is no action
	 *         available yet, e.g. because the user hasn't clicked a button yet.
	 *         In this case, clients should wait a few milliseconds and call
	 *         this method again.
	 */
	public char getAction()
	{
		try
		{
			if (skipFlag)
			{
				if (VERBOSE)
				{
					System.err.println("Bridge state/pressed button: " + bridgeState + "/s");
				}
				skipFlag = false;
				waitingForReturnFromSkip = currentID;
				return 's';
			}
			switch (bridgeState)
			{
				case 'n':
				{
					if (VERBOSE)
					{
						// System.err.println("Bridge state/pressed button: n/n");
					}
					return 'n';
				}
				case 'p':
				{
					if (VERBOSE)
					{
						System.err.println("Bridge state/pressed button: p/n");
					}
					return 'n';
				}
				case 'q':
				{
					if (VERBOSE)
					{
						System.err.println("Bridge state/pressed button: q/n");
					}
					return 'n';
				}
				case 'c':
				{
					if (VERBOSE)
					{
						System.err.println("Bridge state/pressed button: c/c");
					}
					bridgeState = 'n';
					return 'c';
				}
				case 'f':
				{
					if (VERBOSE)
					{
						System.err.println("Bridge state/pressed button: f/f");
					}
					bridgeState = 'n';
					return 'f';
				}
				case 'l':
				{
					if (VERBOSE)
					{
						System.err.println("Bridge state/pressed button: l/c");
					}
					bridgeState = 'l';
					return 'c';
				}
				case 't':
				{
					if (VERBOSE)
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
						if (VERBOSE)
						{
							System.err.println("Bridge state/pressed button: s/n");
						}
						skipID = -1;
						bridgeState = 'n';
						KahinaRunner.processEvent(new KahinaSelectionEvent(currentID));
						return 'n';
					} else
					{
						if (VERBOSE)
						{
							System.err.println("Bridge state/pressed button: s/c");
						}
						return 'c';
					}
				}
				case 'a':
				{
					if (VERBOSE)
					{
						System.err.println("Bridge state/pressed button: a/a");
					}
					return 'a';
				}
				default:
				{
					if (VERBOSE)
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
	protected void processWarnEvent(KahinaWarnEvent e)
	{
		bridgeState = 'n';
	}

	@Override
	protected synchronized void processControlEvent(KahinaControlEvent e)
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
	protected void processSelectionEvent(KahinaSelectionEvent e)
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

	@Override
	protected void processSkipPointMatch(int nodeID, KahinaBreakpoint bp)
	{
		skipFlag = true;
	}

	@Override
	protected void processCreepPointMatch(int nodeID, KahinaBreakpoint bp)
	{
		// no change if we are in leap or skip mode anyway
		if (bridgeState != 's' && bridgeState != 't' && bridgeState != 'l')
		{
			bridgeState = 'c';
		}
	}

	@Override
	protected void processFailPointMatch(int nodeID, KahinaBreakpoint bp)
	{
		// TODO: handle this more elegantly if in skip or leap mode (possibly
		// additional state)
		bridgeState = 'f';
	}

	@Override
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
