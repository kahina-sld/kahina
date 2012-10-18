package org.kahina.lp.bridge;

import java.util.HashMap;
import java.util.Stack;

import org.kahina.core.KahinaException;
import org.kahina.core.KahinaInstance;
import org.kahina.core.bridge.KahinaBridge;
import org.kahina.core.bridge.KahinaStepDescriptionEvent;
import org.kahina.core.control.KahinaActivationEvent;
import org.kahina.core.control.KahinaActivationStatus;
import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaEvent;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.control.KahinaStepUpdateEvent;
import org.kahina.core.control.KahinaSystemEvent;
import org.kahina.core.control.KahinaWarnEvent;
import org.kahina.core.data.agent.KahinaBreakpoint;
import org.kahina.core.data.agent.KahinaControlAgent;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.data.tree.KahinaTreeEvent;
import org.kahina.core.data.tree.KahinaTreeEventType;
import org.kahina.core.gui.event.KahinaSelectionEvent;
import org.kahina.lp.LogicProgrammingInstance;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.LogicProgrammingStep;
import org.kahina.lp.LogicProgrammingStepType;
import org.kahina.lp.control.ControlAgentType;
import org.kahina.lp.control.LogicProgrammingAgentMatchEvent;
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

	// always contains the internal ID of the step one of whose ports we
	// encountered most recently
	protected int currentID = -1;

	// always contains the internal ID of the step which, if a call occurs, will
	// be the parent of the new step
	// TODO we can move this to the tree behavior so bridge doesn't have to access the tree
	protected int parentCandidateID = -1;

	// always contains the internal ID of the selected step
	protected int selectedID = -1;

	// store the state of the bridge, determining the next result of getPressedButton()
	// and the activation pattern of the GUI's control buttons
	private volatile char bridgeState = '0';

	// used to hand on skip commands to the logic programming system
	protected boolean skipFlag = false;
	protected int waitingForReturnFromSkip = -1;

	// in skip mode, this is the internal step ID of the step we are skipping
	protected int skipID = -1;

	protected LogicProgrammingState state;

	public LogicProgrammingBridge(LogicProgrammingInstance<?, ?, ?, ?> kahina)
	{
		super(kahina);
		this.state = (LogicProgrammingState) kahina.getState();
		stepIDConv = new HashMap<Integer, Integer>();
		setBridgeState('n');
		kahina.registerSessionListener(KahinaEventTypes.SELECTION, this);
		kahina.registerSessionListener(KahinaEventTypes.WARN, this);
	    kahina.registerInstanceListener(KahinaEventTypes.SYSTEM, this);
		kahina.registerSessionListener("LP agent match", this);
		if (VERBOSE)
			System.err.println("new LogicProgrammingBridge()");
	}
	
    public void deregister()
    {
        super.deregister();
        kahina.deregisterSessionListener(KahinaEventTypes.SELECTION, this);
        kahina.deregisterSessionListener(KahinaEventTypes.WARN, this);
        kahina.deregisterInstanceListener(KahinaEventTypes.SYSTEM, this);
        kahina.deregisterSessionListener("LP agent match", this);
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
			kahina.dispatchEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.SET_GOAL_DESC, stepID, description));
			currentID = stepID;
			state.consoleMessage(stepID, extID, LogicProgrammingStepType.CALL, consoleMessage);
			if (VERBOSE)
				System.err.println("//LogicProgrammingBridge.registerStepInformation(" + extID + ",\"" + type + ",\"" + description + "\")");
		} 
		catch (Exception e)
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
			kahina.dispatchEvent(new KahinaTreeEvent(KahinaTreeEventType.LAYER, convertStepID(extID), layer));
		} 
		catch (Exception e)
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
			kahina.dispatchEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_CALL, stepID, parentCandidateID));
			currentID = stepID;
			parentCandidateID = stepID;
			if (VERBOSE)
			{
				System.err.println("Bridge state: " + getBridgeState());
			}
			if (VERBOSE)
			{
				System.err.println("Selecting if paused...");
			}
			updateControlElementActivations();
			maybeUpdateStepCount(true);
            kahina.dispatchEvent(new KahinaStepUpdateEvent(stepID));
			selectIfPaused(stepID);
			if (VERBOSE)
			{
				System.err.println("//LogicProgrammingBridge.call(" + extID + ")");
			}
		} 
        catch (Exception e)
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
					System.err.println("Looking up parent of " + id + " in call tree");
				}
				id = stepIDConv.get(state.get(callTree.getParent(id)).getExternalID());

				if (id == -1)
				{
					throw new KahinaException("Unexpected redo of " + lastStepID + " under " + parentCandidateID + ".");
				}
			} 
			while (id != parentCandidateID);

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
				kahina.dispatchEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_REDO, id));

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
			
	        updateControlElementActivations();

			maybeUpdateStepCount(true);
            kahina.dispatchEvent(new KahinaStepUpdateEvent(newStepID));
			selectIfPaused(newStepID);
		} 
		catch (Exception e)
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
			//waitingForReturnFromSkip has done its duty here
			if (stepID == waitingForReturnFromSkip)
			{
				waitingForReturnFromSkip = -1;
			}
			if (deterministic)
			{
				kahina.dispatchEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_DET_EXIT, stepID));
			} 
			else
			{
				kahina.dispatchEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_NONDET_EXIT, stepID));
			}
			currentID = stepID;
			parentCandidateID = state.getSecondaryStepTree().getParent(stepID);

			// relabel node
			if (newDescription != null)
			{
				kahina.dispatchEvent(new KahinaStepDescriptionEvent(stepID, newDescription));
			}

			// create console message
			LogicProgrammingLineReference reference = state.getConsoleLineRefForStep(stepID);
			if (reference != null)
			{
				int port;
				if (deterministic)
				{
					port = LogicProgrammingStepType.DET_EXIT;
				} 
				else
				{
					port = LogicProgrammingStepType.EXIT;
				}
				if (newConsoleMessage == null)
				{
					// use old text
					state.consoleMessage(reference.generatePortVariant(port));
				} 
				else
				{
					state.consoleMessage(stepID, extID, port, newConsoleMessage);
				}
			}

			// Stop autocomplete/leap when we're done. Also, set to creep so
			// we're sure to see the result and get the prompt back.
			if (isQueryRoot(stepID))
			{
				kahina.dispatchEvent(new KahinaSelectionEvent(stepID));
				if (deterministic)
				{
					setBridgeState('c');
				}
			}

	        updateControlElementActivations();
	         
			maybeUpdateStepCount(false);
			//let control agents act
            kahina.dispatchEvent(new KahinaStepUpdateEvent(stepID));
			selectIfPaused(stepID);
		} 
		catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}

	/**
	 * Call to check if a given step has the special property of being the query
	 * root, i.e. when it exits (deterministically) or fails, the whole session
	 * is over.
	 * 
	 * @param stepID
	 * @return
	 */
	protected boolean isQueryRoot(int stepID)
	{
		return stepID == state.getStepTree().getRootID(); // TODO memorize?
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
			kahina.dispatchEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_FAIL, stepID));
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
				kahina.dispatchEvent(new KahinaSelectionEvent(stepID));
				setBridgeState('c');
			}
			
            updateControlElementActivations();
			maybeUpdateStepCount(false);
            kahina.dispatchEvent(new KahinaStepUpdateEvent(stepID));
			selectIfPaused(stepID);
		} 
		catch (Exception e)
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
			kahina.dispatchEvent(new LogicProgrammingBridgeEvent(LogicProgrammingBridgeEventType.STEP_EXCEPTION, stepID));
			currentID = stepID;
			parentCandidateID = state.getSecondaryStepTree().getParent(stepID);

			state.exceptionConsoleMessage(stepID, extID, message);

			// Stop autocomplete/leap when we're done. Also, set to creep so
			// we're sure to see the result and get the prompt back.
			if (isQueryRoot(stepID))
			{
				kahina.dispatchEvent(new KahinaSelectionEvent(stepID));
				setBridgeState('c');
			}
            updateControlElementActivations();
			maybeUpdateStepCount(false);
            kahina.dispatchEvent(new KahinaStepUpdateEvent(stepID));
			selectIfPaused(stepID);
		} 
		catch (Exception e)
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
		} 
		catch (Exception e)
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
			setBridgeState('n');
			maybeUpdateStepCount(false);
			kahina.dispatchEvent(new KahinaSelectionEvent(currentID));
		} 
		catch (Exception e)
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
			kahina.dispatchEvent(new KahinaSelectionEvent(currentID));
		} 
		catch (Exception e)
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
					System.err.println(this + ".getAction() == 's', skipFlag := false, waitingForReturnFromSkip := " + currentID);
				}
			    skipFlag = false;
			    waitingForReturnFromSkip = currentID;
			    if (getBridgeState() == 't') return 'c';
			    else return 's';
			}
			switch (getBridgeState())
			{
    			case 'n':
    			{
                    if (VERBOSE)
                    {
                        System.err.println(this + ".getAction() == 'n', in idle mode");
                    }
    				return 'n';
    			}
    			case 'p':
    			{
    				if (VERBOSE)
    				{
    					System.err.println(this + ".getAction() == 'n', in pause mode");
    				}
    				return 'n';
    			}
    			case 'q':
    			{
                    if (VERBOSE)
                    {
                        System.err.println(this + ".getAction() == 'n', in paused skip mode");
                    }
    				return 'n';
    			}
    			case 'c':
    			{
                    if (VERBOSE)
                    {
                        System.err.println(this + ".getAction() == 'c', back to idle mode");
                    }
    				setBridgeState('n');      
    				return 'c';
    			}
    			case 'f':
    			{
                    if (VERBOSE)
                    {
                        System.err.println(this + ".getAction() == 'f', back to idle mode");
                    }
    				setBridgeState('n');
    				return 'f';
    			}
    			case 'l':
    			{
                    if (VERBOSE)
                    {
                        System.err.println(this + ".getAction() == 'c', in leap mode");
                    }
    				return 'c';
    			}
    			case 't':
    			{
    			    if (skipID == currentID)
                    {
                        if (VERBOSE)
                        {
                            System.err.println(this + ".getAction() == 'n', back in idle mode after auto-complete");
                        }
                        skipID = -1;
                        setBridgeState('n');
                        kahina.dispatchEvent(new KahinaSelectionEvent(currentID));
                        return 'n';
                    } 
                    else
                    {
                        if (VERBOSE)
                        {
                            System.err.println(this + ".getAction() == 'c', in auto-complete mode");
                        }
                        return 'c';
                    }
    			}
    			case 's':
    			{
    				if (skipID == currentID)
    				{
                        if (VERBOSE)
                        {
                            System.err.println(this + ".getAction() == 'n', back in idle mode because skip complete");
                        }
    					skipID = -1;
    					setBridgeState('n');
    					kahina.dispatchEvent(new KahinaSelectionEvent(currentID));
    					return 'n';
    				} 
                    else
    				{
                        if (VERBOSE)
                        {
                            System.err.println(this + ".getAction() == 'c', in skip mode");
                        }
    					return 'c';
    				}
    			}
    			case 'a':
    			{
                    if (VERBOSE)
                    {
                        System.err.println(this + ".getAction() == 'a', in abort mode");
                    }
    				return 'a';
    			}
    			default:
    			{
                    if (VERBOSE)
                    {
                        System.err.println(this + ".getAction() == 'n', back to idle mode (default behavior)");
                    }
    				setBridgeState('n');
    				return 'n';
    			}
			}
		} 
		catch (Exception e)
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
		/*if (VERBOSE)
			System.err.println("LogicProgrammingBridge.convertStepID(" + extID + ")");*/
		if (extID == -1)
		{
            if (VERBOSE)
                System.err.println("LogicProgrammingBridge.convertStepID(-1) = -1");
			return -1;
		}
		Integer intID = stepIDConv.get(extID);
		/*if (VERBOSE)
		{
			System.err.println("stepIDConv.get(" + extID + ")=" + intID);
		}*/
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

	protected boolean isPaused()
	{
		return getBridgeState() == 'n' || getBridgeState() == 'c';
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
		if (isPaused())
		{
			kahina.dispatchEvent(new KahinaSelectionEvent(stepID));
		}
	}

	/**
	 * Update step count in GUI. In fast-forward mode, this is only done every
	 * 100 steps to reduce graphics load.
	 * 
	 * @param firstEncounter
	 *            Should be true at call and redo goals, false otherwise. The
	 *            latter avoids unnecessary multiple updates per step.
	 */
	protected void maybeUpdateStepCount(boolean firstEncounter)
	{
		int stepCount = state.getStepCount();
		// TODO || currentID < 3 is a HACK, because when the top goal completes
		// while Kahina is fast-forwarding, the client might wait prompting for
		// more solutions rather than calling the end method, thus Kahina still
		// thinks it's fast-forwarding and doesn't update without this hack. The
		// root problem is with how to handle aforementioned solution prompts,
		// so when we solve that, we should be able to get rid of this hack here
		// too.
		if (isPaused() || (stepCount % 100 == 0 && firstEncounter) || currentID < 3)
		{
			kahina.dispatchEvent(new KahinaSystemEvent(KahinaSystemEvent.NODE_COUNT, stepCount));
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
    public void processEvent(KahinaEvent e)
    {
        if (e instanceof LogicProgrammingAgentMatchEvent)
        {
            LogicProgrammingAgentMatchEvent match = (LogicProgrammingAgentMatchEvent) e;
            System.err.println("Match detected by agent " + match.getAgent());
            switch (match.getAgentType())
            {
                case BREAK_AGENT:
                {
                    performBreakAction("Agent \"" + match.getAgent().getName() + "\"");
                    break;
                }
                case CREEP_AGENT:
                {
                    performCreepAction("Agent \"" + match.getAgent().getName() + "\"");
                    break;
                }
                case COMPLETE_AGENT:
                {
                    performCompleteAction("Agent \"" + match.getAgent().getName() + "\"");
                    break;
                }
                case SKIP_AGENT:
                {
                    performSkipAction("Agent \"" + match.getAgent().getName() + "\"");
                    break;
                }
                case FAIL_AGENT:
                {
                    performFailAction("Agent \"" + match.getAgent().getName() + "\"");
                    break;
                }
            }
        }
        else
        {
            super.processEvent(e);
        }
    }

	@Override
	protected void processSystemEvent(KahinaSystemEvent e)
	{
		if (e.getSystemEventType() == KahinaSystemEvent.QUIT)
		{
			setBridgeState('a');
		}
	}

	@Override
	protected void processWarnEvent(KahinaWarnEvent e)
	{
		setBridgeState('n');
		maybeUpdateStepCount(false);
		selectIfPaused(currentID);
	}

	@Override
	protected synchronized void processControlEvent(KahinaControlEvent e)
	{
        if (VERBOSE) System.err.println(this + "e.processControlEvent(" + e + ")");
		// TODO update chart when exiting leap/skip. Gah.
		String command = e.getCommand();
		if (command.equals("creep"))
		{
		    performCreepAction("User");
		} 
		else if (command.equals("stop"))
		{
			if (getBridgeState() == 'p')
			{
				skipID = -1;
				setBridgeState('c');
			} 
			else if (getBridgeState() == 'q')
			{
				skipID = -1;
				setBridgeState('c');
			} 
			else if (getBridgeState() == 'l')
			{
				skipID = -1;
				setBridgeState('c');
			}
		} 
		else if (command.equals("fail"))
		{
            performFailAction("User");
		} 
		else if (command.equals("auto-complete"))
		{
		    performCompleteAction("User");
		} 
		else if (command.equals("skip"))
		{
	        performSkipAction("User");
		} 
		else if (command.equals("leap"))
		{
			if (getBridgeState() == 'n')
			{
				setBridgeState('l');
			} 
			else if (getBridgeState() == 'p')
			{
				setBridgeState('l');
				skipID = -1;
			} 
			else if (getBridgeState() == 'q')
			{
				setBridgeState('l');
				skipID = -1;
			}
			else if (getBridgeState() == 'l')
			{
			    performBreakAction("User");
			    state.breakpointConsoleMessage(currentID, "User starts leap at step " + state.get(currentID).externalID + ".");
			}
		} 
		else if (command.equals("(un)pause"))
		{
			performBreakAction("User");
		} 
		else if (command.equals("abort"))
		{
			setBridgeState('a');
			if (currentID != -1)
			{
			    state.breakpointConsoleMessage(currentID, "User causes abort at step " + state.get(currentID).externalID + ". Trace closed.");
			}
		}
	}

	protected boolean canSkipOrAutocomplete()
	{
	    //TODO: think about how to handle this consistently for selectedID != candidateID
		int candidateID = currentID;
		/*if (selectedID == -1 || getBridgeState() != 'n')
		{
			candidateID = currentID;
		} 
        else
		{
			candidateID = selectedID;
		}*/
		/*state.breakpointConsoleMessage(candidateID,"calling canSkipOrAutocomplete(" 
		                                         + state.get(currentID).externalID + "/" 
		                                         + state.get(selectedID).externalID +") = " 
		                                         + state.get(candidateID).externalID);*/
		int status = state.getStepTree().getNodeStatus(candidateID);
		return (status == LogicProgrammingStepType.CALL || status == LogicProgrammingStepType.REDO);
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
			// TODO only jump on doubleclick to reduce user surprise and risk of event cycles
			kahina.dispatchEvent(new KahinaSelectionEvent(linkTarget));
		}
	}

	protected void performSkipAction(String agentString)
	{
        if (canSkipOrAutocomplete())
        {
            skipFlag = true;
            state.breakpointConsoleMessage(currentID, agentString + " causes skip at step " + state.get(currentID).externalID + ".");
        } 
        else
        {
            state.breakpointConsoleMessage(currentID, agentString + " attempted to skip at step " + state.get(currentID).externalID + ", but skip operation was disallowed.");
        }
	}
    
    protected void performCompleteAction(String agentString)
    {
        if (getBridgeState() == 'n')
        {
            if (canSkipOrAutocomplete())
            {
                setBridgeState('t');
                //TODO: rethink and debug the selectedID case
                //if (selectedID == -1)
                {
                    skipFlag = true;
                    state.breakpointConsoleMessage(currentID, agentString + " causes auto-complete at step " + state.get(currentID).externalID + ".");
                    skipID = currentID;
                } 
                /*else
                {
                    state.breakpointConsoleMessage(selectedID, agentString + " causes auto-complete at step " + state.get(selectedID).externalID + ".");
                    skipID = selectedID;
                }*/
            } 
            else
            {
                state.breakpointConsoleMessage(currentID, agentString + " attempted auto-complete at " + state.get(currentID).externalID + ", but auto-complete operation was disallowed.");
            }
        } 
        else if (getBridgeState() == 'p')
        {
            setBridgeState('t');
        } 
        //do not let control agents abort auto-complete operations!
        else if (getBridgeState() == 't' && "User".equals(agentString))
        {
            setBridgeState('n');
        }
    }

	protected void performCreepAction(String agentString)
	{
		// no change if we are in leap or skip mode anyway
		if (getBridgeState() != 's' && getBridgeState() != 't' && getBridgeState() != 'l')
		{
			setBridgeState('c');
			if (!"User".equals(agentString))
			{
			    state.breakpointConsoleMessage(currentID, agentString + " causes creep at step " + state.get(currentID).externalID + ".");
			}
		}
	}

	protected void performFailAction(String agentString)
	{
		// TODO: handle this more elegantly if in skip or leap mode (possibly additional state)
		setBridgeState('f');
        state.breakpointConsoleMessage(currentID,  agentString + " causes failure at step " + state.get(currentID).externalID + ".");
	}

	protected void performBreakAction(String agentString)
	{
		// is used for the pause mode of auto-completion as well as for breakpoints
	    // TODO: in case of non-user agent, mark matching node in the breakpoint's signal color
		if (getBridgeState() == 't')
		{
			setBridgeState('p');
	        state.breakpointConsoleMessage(currentID, agentString + " causes auto-complete break at step " + state.get(currentID).externalID + ".");
	        kahina.dispatchEvent(new KahinaSelectionEvent(currentID));
		} 
		else if (getBridgeState() == 's')
		{
			setBridgeState('q');
	        state.breakpointConsoleMessage(currentID, agentString + " causes skip break at step " + state.get(currentID).externalID + ".");
	        kahina.dispatchEvent(new KahinaSelectionEvent(currentID));
		} 
		else if (getBridgeState() == 'l')
		{
			setBridgeState('n');
	        state.breakpointConsoleMessage(currentID, agentString + " stops leap at step " + state.get(currentID).externalID + ".");
	        kahina.dispatchEvent(new KahinaSelectionEvent(currentID));
		}
        else if (getBridgeState() == 'p')
        {
            setBridgeState('t');
            state.breakpointConsoleMessage(currentID, agentString + " unpauses auto-complete at step " + state.get(currentID).externalID + ".");
        } 
        else if (getBridgeState() == 'q')
        {
            setBridgeState('s');
            state.breakpointConsoleMessage(currentID, agentString + " unpauses skip at step " + state.get(currentID).externalID + ".");
        }
	}
	
	private void updateControlElementActivations()
	{
	    switch (bridgeState)
        {
            case 'n':
            {
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("creep",KahinaActivationStatus.ACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("(un)pause",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("fail",KahinaActivationStatus.ACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("leap",KahinaActivationStatus.ACTIVE));
                if (canSkipOrAutocomplete())
                {
                    kahina.dispatchInstanceEvent(new KahinaActivationEvent("skip",KahinaActivationStatus.ACTIVE));
                    kahina.dispatchInstanceEvent(new KahinaActivationEvent("auto-complete",KahinaActivationStatus.ACTIVE));
                }
                else
                {
                    kahina.dispatchInstanceEvent(new KahinaActivationEvent("skip",KahinaActivationStatus.INACTIVE));
                    kahina.dispatchInstanceEvent(new KahinaActivationEvent("auto-complete",KahinaActivationStatus.INACTIVE));
                }
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("abort",KahinaActivationStatus.ACTIVE));
                break;
            }
            case 'c':
            {
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("creep",KahinaActivationStatus.ACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("(un)pause",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("fail",KahinaActivationStatus.ACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("leap",KahinaActivationStatus.ACTIVE));
                if (canSkipOrAutocomplete())
                {
                    kahina.dispatchInstanceEvent(new KahinaActivationEvent("skip",KahinaActivationStatus.ACTIVE));
                    kahina.dispatchInstanceEvent(new KahinaActivationEvent("auto-complete",KahinaActivationStatus.ACTIVE));
                }
                else
                {
                    kahina.dispatchInstanceEvent(new KahinaActivationEvent("skip",KahinaActivationStatus.INACTIVE));
                    kahina.dispatchInstanceEvent(new KahinaActivationEvent("auto-complete",KahinaActivationStatus.INACTIVE));
                }
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("abort",KahinaActivationStatus.ACTIVE));
                break;
            }
            case 's':
            {
                /*kahina.dispatchInstanceEvent(new KahinaActivationEvent("creep",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("skip",KahinaActivationStatus.PRESSED));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("(un)pause",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("fail",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("leap",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("abort",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("auto-complete",KahinaActivationStatus.INACTIVE));
                break;*/
                
                //same behavior as t for now; this will change once we reactivate auto-completion of selected steps
            }
            case 't':
            {
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("creep",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("skip",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("(un)pause",KahinaActivationStatus.ACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("fail",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("leap",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("abort",KahinaActivationStatus.ACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("auto-complete",KahinaActivationStatus.PRESSED));
                break;
            }
            case 'p':
            {
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("creep",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("skip",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("(un)pause",KahinaActivationStatus.PRESSED));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("fail",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("leap",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("abort",KahinaActivationStatus.ACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("auto-complete",KahinaActivationStatus.PRESSED));
                break;
            }
            case 'q':
            {
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("creep",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("skip",KahinaActivationStatus.PRESSED));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("(un)pause",KahinaActivationStatus.PRESSED));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("fail",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("leap",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("abort",KahinaActivationStatus.ACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("auto-complete",KahinaActivationStatus.INACTIVE));
                break;
            }
            case 'l':
            {
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("creep",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("skip",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("(un)pause",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("fail",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("leap",KahinaActivationStatus.PRESSED));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("abort",KahinaActivationStatus.ACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("auto-complete",KahinaActivationStatus.INACTIVE));
                break;
            }
            case 'a':
            {
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("creep",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("skip",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("(un)pause",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("fail",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("leap",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("abort",KahinaActivationStatus.INACTIVE));
                kahina.dispatchInstanceEvent(new KahinaActivationEvent("auto-complete",KahinaActivationStatus.INACTIVE));
                break;
            }
        }
	}

    public void setBridgeState(char bridgeState)
    {
        if (VERBOSE) 
            System.err.println(this + ".setBridgeState(" + bridgeState + ")");
        //reflect changes to the bridge state in the control button activation pattern
        if (this.bridgeState != bridgeState)
        {
            this.bridgeState = bridgeState;
            updateControlElementActivations();
        }
    }

    public char getBridgeState()
    {
        if (VERBOSE) System.err.println(this + ".getBridgeState() == " + bridgeState);
        return bridgeState;
    }
}
