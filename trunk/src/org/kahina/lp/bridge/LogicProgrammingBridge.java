package org.kahina.lp.bridge;

import java.util.HashMap;
import java.util.Stack;

import org.kahina.core.KahinaException;
import org.kahina.core.KahinaInstance;
import org.kahina.core.bridge.KahinaBridge;
import org.kahina.core.bridge.KahinaBridgePauseEvent;
import org.kahina.core.bridge.KahinaStepDescriptionEvent;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.control.KahinaSystemEvent;
import org.kahina.core.control.KahinaWarnEvent;
import org.kahina.core.data.breakpoint.KahinaBreakpoint;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.data.tree.KahinaTreeEvent;
import org.kahina.core.data.tree.KahinaTreeEventType;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.lp.LogicProgrammingStepType;
import org.kahina.lp.data.text.LogicProgrammingLineReference;

/**
 * The logic programming bridge is the most important object that you work with
 * when you connect a logic programming system to Kahina that has tracing
 * capabilities following the procedure box model. Its public methods take
 * information about the control flow and properties of procedure boxes ("steps"
 * in Kahina terminology) and turn it into information that can be displayed by
 * Kahina's views.
 * 
 * <p>
 * Logic programming systems that have been connected to Kahina using
 * specialized subclasses of {@link LogicProgrammingBridge} so far include
 * TRALE, QType and, experimentally, SWI-Prolog and SICStus Prolog. The
 * connection typically involves some Prolog code that hooks into the tracer of
 * the respective LP system and calls Kahina's Java API via a Prolog/Java
 * connection layer such as Jasper or JPL.
 * 
 * <p>
 * A Kahina "session" corresponds to the execution of one query in a LP system.
 * A new bridge is created, and all views are cleared, when a new session is
 * started. A session is started by calling the
 * {@link KahinaInstance#startNewSession()} method of an instance of the
 * {@link KahinaInstance} subclass that is specialized for the respective LP
 * system. This method returns the bridge for that session. The bridge is then
 * used to transmit all tracing information about a query to Kahina.
 */
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
	protected int skipID = -1;

	protected LogicProgrammingState state;

	public LogicProgrammingBridge(LogicProgrammingInstance<?,?,?> kahina)
	{
		super(kahina);
		this.state = (LogicProgrammingState) kahina.getState();
		stepIDConv = new HashMap<Integer, Integer>();
		kahina.getControl().registerListener(KahinaEventTypes.SYSTEM, this);
		kahina.getControl().registerListener(KahinaEventTypes.SELECTION, this);
		kahina.getControl().registerListener(KahinaEventTypes.WARN, this);
		if (VERBOSE)
			System.err.println("new LogicProgrammingBridge()");
	}

	/**
	 * For each new procedure box that is created, this method or one of its
	 * variants must first be called. It is separate from {@link #call(int)} for
	 * historic reasons and for flexibility, e.g. it can be overloaded with
	 * various arguments representing all kinds of information about a step
	 * without touching the call method. Note however that information about a
	 * step that is not absolutely central, such as source code locations,
	 * should be sent to Kahina using specialized methods following the call to
	 * the step method.
	 * 
	 * @param extID
	 *            An ID identifying the procedure box uniquely.
	 * @param type
	 *            A string identifying the type of the step, e.g. a Prolog
	 *            predicate identifier such as {@code append/3}. Will be used
	 *            for categorizing and counting steps in the profiler.
	 * @param description
	 *            A full description of the step, such as
	 *            {@code append([1,2],[3,4],X)}. Will be used for labeling nodes
	 *            in the control flow graph.
	 * @param consoleMessage
	 *            A more extensive description of the (type of) the step, such
	 *            as a prose description of what {@code append/3} does. Will be
	 *            displayed in the message console.
	 */
	public void step(int extID, String type, String description, String consoleMessage)	
	{
		try
		{
			if (VERBOSE)
				System.err.println("LogicProgrammingBridge.registerStepInformation(" + extID + ",\"" + type + ",\"" + description + "\")");
			int stepID = convertStepID(extID);
			LogicProgrammingStep step = state.get(stepID);
			step.setGoalDesc(type); // TODO Also save goal in step in a separate field?
			if (waitingForReturnFromSkip != -1)
			{
				state.hideStep(stepID);
			}
			if (currentID != -1)
			{
				step.setSourceCodeLocation(state.get(currentID).getSourceCodeLocation());
			}
			state.store(stepID, step);
			// Set node label:
			kahina.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.SET_GOAL_DESC, stepID, description));
			currentID = stepID;
			state.consoleMessage(stepID, extID, LogicProgrammingStepType.CALL, consoleMessage);
			if (VERBOSE)
				System.err.println("//LogicProgrammingBridge.registerStepInformation(" + extID + ",\"" + type + ",\"" + description + "\")");
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	/**
	 * Variant of {@link #step(int, String, String, String)} that uses the same
	 * string for description and console message.
	 * 
	 * @param extID
	 * @param predicate
	 * @param description
	 */
	public void step(int extID, String predicate, String description)
	{
		step(extID, predicate, description, description);
	}

	/**
	 * Variant of {@link #step(int, String, String, String)} that uses the same
	 * string for type, description and console message.
	 * 
	 * @param extID
	 * @param type
	 */
	public void step(int extID, String type)
	{
		step(extID, type, type);
	}

	/**
	 * Registers the source code location connected to a step for display in the
	 * source code view. TODO: Different ports of a procedure box may have
	 * different source code locations.
	 * 
	 * @param extID
	 * @param absolutePath
	 * @param lineNumber
	 */
	public void registerStepSourceCodeLocation(int extID, String absolutePath, int lineNumber)
	{
		try
		{
			if (VERBOSE)
				System.err.println("LogicProgrammingBridge.registerStepSourceCodeLocation(" + extID + ",\"" + absolutePath + "\"," + lineNumber + ")");
			int stepID = convertStepID(extID);
			LogicProgrammingStep step = state.get(stepID);
			step.setSourceCodeLocation(new KahinaSourceCodeLocation(absolutePath, lineNumber - 1));
			currentID = stepID;
			state.store(stepID, step);
			selectIfPaused(stepID);
		} 
		catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void registerLayer(int extID, int layer)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".registerLayer(" + extID + ", " + layer + ")");
		}
		try
		{
			kahina.processEvent(new KahinaTreeEvent(KahinaTreeEventType.LAYER, convertStepID(extID), layer));
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	/**
	 * Called, typically following a call to
	 * {@link #step(int, String, Stirng, String)} very soon, to indicate that
	 * the call port of the procedure box with the given ID has been reached.
	 * This will cause the corresponding node to appear in Kahina's control flow
	 * graph.
	 * 
	 * @param extID
	 */
	public void call(int extID)
	{
		try
		{
			if (VERBOSE)
			{
				System.err.println("LogicProgrammingBridge.call(" + extID + ")");
				System.err.println("Converting step ID...");
			}
			int stepID = convertStepID(extID);
			if (VERBOSE)
			{
				System.err.println("Parent ID: " + parentCandidateID);
			}
			// used by tree behavior:
			kahina.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_CALL, stepID, parentCandidateID));
			// used by node counter:
			kahina.processEvent(new KahinaSystemEvent(KahinaSystemEvent.NODE_COUNT, stepID));
			currentID = stepID;
			parentCandidateID = stepID;
			if (VERBOSE)
			{
				System.err.println("Bridge state: " + bridgeState);
			}
			if (VERBOSE)
			{
				System.err.println("Selecting if paused...");
			}
			selectIfPaused(stepID);
			if (VERBOSE)
			{
				System.err.println("//LogicProgrammingBridge.call(" + extID + ")");
			}
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	/**
	 * Called to indicate that the redo port of the procedure box with the given
	 * ID has been reached. This will create a copy of the original step in
	 * Kahina's data model, and a new node in Kahina's control flow graph,
	 * distinct from nodes created by previous calls and redos of the box, but
	 * labeled with the same ID. Internally, Kahina uses a different set of IDs
	 * to distinguish different "instantiations" of the same procedure box when
	 * it is redone.
	 * 
	 * @param extID
	 */
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

			if (VERBOSE)
			{
				System.err.println("Current parent candidate: " + parentCandidateID);
			}

			// Collect the steps we need to backtrack into, from the one being
			// redone up until (and excluding) the current parent candidate:
			do
			{
				if (VERBOSE)
				{
					System.err.println("Pushing " + id + " onto redo stack.");
				}

				redoStack.push(id);

				if (id == parentCandidateID)
				{
					break;
				}

				// Get the internal ID of the parent, or that of its copy if it
				// already has one:
				if (VERBOSE)
				{
					System.err.println("Looking up parent of " + id + " in " + callTree);
				}
				id = stepIDConv.get(state.get(callTree.getParent(id)).getExternalID());

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
				LogicProgrammingStep lastStep = state.get(id);
				LogicProgrammingStep newStep = lastStep.copy();
				newStep.incrementRedone();
				newStepID = state.nextStepID();
				state.store(newStepID, newStep);
				stepIDConv.put(lastStep.getExternalID(), newStepID);
				kahina.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_REDO, id));

				LogicProgrammingLineReference reference = state.getConsoleLineRefForStep(id);
				if (reference != null)
				{
					// TODO Do we want to select by external rather than
					// internal ID in the console?
					state.consoleMessage(reference.generatePortVariant(LogicProgrammingStepType.REDO).generateIDVariant(newStepID));
				}
			}

			currentID = newStepID;
			parentCandidateID = newStepID;

			selectIfPaused(newStepID);
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void exit(int extID, boolean deterministic)
	{
		exit(extID, deterministic, null, null);
	}

	public void exit(int extID, boolean deterministic, String newDescription)
	{
		exit(extID, deterministic, newDescription, newDescription);
	}

	/**
	 * Called to indicate that the exit port of the procedure box with the given
	 * ID has been reached.
	 * 
	 * @param extID
	 * @param deterministic
	 *            {@code true} if the box has exited deterministically, i.e. is
	 *            guaranteed never to be redone again. If you cannot get this
	 *            information from your LP system, the recommended default is
	 *            {@code false}.
	 */
	public void exit(int extID, boolean deterministic, String newDescription, String newConsoleMessage)
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
				kahina.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_DET_EXIT, stepID));
			} 
			else
			{
				kahina.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_NONDET_EXIT, stepID));
			}
			currentID = stepID;
			parentCandidateID = state.getSecondaryStepTree().getParent(stepID);

			// relabel node
			if (newDescription != null)
			{
				kahina.processEvent(new KahinaStepDescriptionEvent(stepID, newDescription));
			}

			// create console message
			LogicProgrammingLineReference reference = state.getConsoleLineRefForStep(stepID);
			if (reference != null)
			{
				int port;
				if (deterministic)
				{
					port = LogicProgrammingStepType.DET_EXIT;
				} else
				{
					port = LogicProgrammingStepType.EXIT;
				}
				if (newConsoleMessage == null)
				{
					// use old text
					state.consoleMessage(reference.generatePortVariant(port));
				} else
				{
					state.consoleMessage(stepID, extID, port, newConsoleMessage);
				}
			}

			// Stop autocomplete/leap when we're done. Also, set to creep so
			// we're sure to see the result and get the prompt back.
			if (isQueryRoot(stepID))
			{
				kahina.processEvent(new KahinaBridgePauseEvent());
				kahina.processEvent(new KahinaSelectionEvent(stepID));
				if (deterministic)
				{
					bridgeState = 'c';
				}
			}

			selectIfPaused(stepID);
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}
	
	/**
	 * Call to check if a given step has the special property of being the query
	 * root, i.e. when it exits (deterministically) or fails, the whole session
	 * is over.
	 * @param stepID
	 * @return
	 */
	protected boolean isQueryRoot(int stepID)
	{
		return stepID == state.getStepTree().getRootID(); // TODO memoize?
	}

	/**
	 * Called to indicate that the fail port of the procedure box with the given
	 * ID has been reached.
	 * 
	 * @param extID
	 */
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
			kahina.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_FAIL, stepID));
			currentID = stepID;
			parentCandidateID = state.getSecondaryStepTree().getParent(stepID);

			LogicProgrammingLineReference reference = state.getConsoleLineRefForStep(stepID);
			if (reference != null)
			{
				state.consoleMessage(reference.generatePortVariant(LogicProgrammingStepType.FAIL));
			}

			// Stop autocomplete/leap when we're done. Also, set to creep so
			// we're sure to see the result and get the prompt back.
			if (isQueryRoot(stepID))
			{
				kahina.processEvent(new KahinaBridgePauseEvent());
				kahina.processEvent(new KahinaSelectionEvent(stepID));
				bridgeState = 'c';
			}

			selectIfPaused(stepID);
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	/**
	 * Called to indicate that the exception port of the procedure box with the
	 * given ID has been reached.
	 * 
	 * @param extID
	 */
	public void exception(int extID, String message)
	{
		try
		{
			if (VERBOSE)
			{
				System.err.println("LogicProgrammingBridge.exception(" + extID + ")");
			}
			int stepID = convertStepID(extID);
			if (stepID == waitingForReturnFromSkip)
			{
				waitingForReturnFromSkip = -1;
			}
			kahina.processEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_EXCEPTION, stepID));
			currentID = stepID;
			parentCandidateID = state.getSecondaryStepTree().getParent(stepID);

			state.exceptionConsoleMessage(stepID, extID, message);

			// Stop autocomplete/leap when we're done. Also, set to creep so
			// we're sure to see the result and get the prompt back.
			if (isQueryRoot(stepID))
			{
				kahina.processEvent(new KahinaBridgePauseEvent());
				kahina.processEvent(new KahinaSelectionEvent(stepID));
				bridgeState = 'c';
			}

			selectIfPaused(stepID);
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	public void warning(String message)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".warning(" + message + ")");
		}
		// TODO
	}

	/**
	 * Introduces a "secondary edge" to the control flow tree, e.g. for marking
	 * the corresponding blocking step (target) for a given unblocking step
	 * (anchor).
	 * 
	 * @param anchor
	 * @param target
	 */
	public void linkNodes(int anchor, int target)
	{
		try
		{
			state.linkNodes(convertStepID(anchor), convertStepID(target));
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
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
			kahina.processEvent(new KahinaBridgePauseEvent());
			bridgeState = 'n';
			kahina.processEvent(new KahinaSelectionEvent(currentID));
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	/**
	 * Call this to force the GUI to select the indicated step and update, e.g.
	 * before pausing to present the user with a result.
	 * 
	 * @param extID
	 */
	public void select(int extID)
	{
		try
		{
			kahina.processEvent(new KahinaSelectionEvent(currentID));
		} catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
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
				kahina.processEvent(new KahinaBridgePauseEvent());
				bridgeState = 'n';
				return 'c';
			}
			case 'f':
			{
				if (VERBOSE)
				{
					System.err.println("Bridge state/pressed button: f/f");
				}
				kahina.processEvent(new KahinaBridgePauseEvent());
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
					kahina.processEvent(new KahinaBridgePauseEvent());
					bridgeState = 'n';
					kahina.processEvent(new KahinaSelectionEvent(currentID));
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
				kahina.processEvent(new KahinaBridgePauseEvent());
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
			state.store(intID, newStep);
			stepIDConv.put(extID, intID);
		}
		if (VERBOSE)
			System.err.println("LogicProgrammingBridge.convertStepID(" + extID + ") = " + intID);
		return intID;
	}

	/**
	 * Selects the given step and updates the GUI if the debugger is not
	 * currently leaping or autocompleting.
	 */
	protected void selectIfPaused(int stepID)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".selectIfPaused(" + stepID + ")");
		}
		if (bridgeState == 'n')
		{
			kahina.processEvent(new KahinaSelectionEvent(stepID));
		}
	}

	@Override
	protected LogicProgrammingStep generateStep()
	{
		if (VERBOSE)
			System.err.println("LogicProgrammingBridge.generateStep()");
		return new LogicProgrammingStep();
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
		kahina.processEvent(new KahinaBridgePauseEvent());
		bridgeState = 'n';
		selectIfPaused(currentID);
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
				if (canSkipOrAutocomplete())
				{
					bridgeState = 't';
					if (selectedID == -1)
					{
						skipID = currentID;
					} else
					{
						skipID = selectedID;
					}
				} else
				{
					if (VERBOSE)
					{
						System.err.println("WARNING: auto-complete/skip are not valid operations right now.");
					}
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
			if (canSkipOrAutocomplete())
			{
				skipFlag = true;
			} else
			{
				if (VERBOSE)
				{
					System.err.println("WARNING: auto-complete/skip are not valid operations right now.");
				}
			}
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
		} else if (command.equals("abort"))
		{
			bridgeState = 'a';
		}
	}

	protected boolean canSkipOrAutocomplete()
	{
		int candidateID;
		if (selectedID == -1)
		{
			candidateID = currentID;
		} else
		{
			candidateID = selectedID;
		}
		int status = state.getStepTree().getNodeStatus(candidateID);
		return status == LogicProgrammingStepType.CALL || status == LogicProgrammingStepType.REDO;
	}

	@Override
	protected void processSelectionEvent(KahinaSelectionEvent e)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".processSelectionEvent(" + e + ")");
		}
		selectedID = e.getSelectedStep();
		Integer linkTarget = state.getLinkTarget(selectedID);
		if (linkTarget != null)
		{
			// TODO only jump on doubleclick to reduce user surprise and risk
			// of event cycles
			kahina.processEvent(new KahinaSelectionEvent(linkTarget));
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
		kahina.processEvent(new KahinaBridgePauseEvent());
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
