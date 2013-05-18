package org.kahina.qtype.bridge;

import gralej.om.IEntity;
import gralej.om.IList;
import gralej.om.ITag;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.gui.event.KahinaChartUpdateEvent;
import org.kahina.prolog.util.PrologUtil;
import org.kahina.qtype.QTypeDebuggerInstance;
import org.kahina.qtype.QTypeState;
import org.kahina.qtype.QTypeStep;
import org.kahina.qtype.control.QTypeControlEventCommands;
import org.kahina.qtype.data.bindings.QTypeGoal;
import org.kahina.sicstus.bridge.SICStusPrologBridge;
import org.kahina.tralesld.data.fs.TraleSLDFSPacker;
import org.kahina.tralesld.visual.fs.GraleJUtility;

public class QTypeBridge extends SICStusPrologBridge
{
	private static final boolean VERBOSE = false;

	private static final Pattern SENTENCE_PATTERN = Pattern.compile("\\[([^\\]]+)\\]");
	
	QTypeState state;

	private final TraleSLDFSPacker packer;
	
	//temporary variables for tracking the state of the chart
	int currentPosition = 0;
	boolean lexEntryExistenceMode = false;
	int topLexEntryExistenceStep = -1;
	int currentEdge = -1;
	Map<Integer,Integer> edgeToCurrentPosition;
	//int lastRuleNode = -1;
	//private int lastSpanEdge = -1;

	public QTypeBridge(final QTypeDebuggerInstance kahina)
	{
		super(kahina);
	    this.state = kahina.getState();
		packer = new TraleSLDFSPacker();
		edgeToCurrentPosition = new HashMap<Integer,Integer>();
	}

	@Override
	public void step(int extID, String type, String description, String consoleMessage)
	{
	    super.step(extID, type, description, consoleMessage);
	    if (VERBOSE) System.err.println("QTypeBridge.step(" + extID + "," + type + "," + description + "," + consoleMessage + ")");
	       
		// For compile_grammar steps, unregister any previously registered grammar:
		if (description.startsWith("compile_grammar(") && description.endsWith(")"))
		{
			kahina.dispatchEvent(new KahinaControlEvent(QTypeControlEventCommands.REGISTER_GRAMMAR, new Object[] { null }));
		}
		else if (description.startsWith("lc(") && description.endsWith(")"))
		{
		    //for lc/1 steps, register the sentence
			Matcher matcher = SENTENCE_PATTERN.matcher(description.substring(3, description.length() - 1));
			if (matcher.matches())
			{
			    List<String> wordList = PrologUtil.parsePrologStringList(matcher.group(0));
				kahina.dispatchEvent(new KahinaControlEvent(QTypeControlEventCommands.REGISTER_SENTENCE, new Object[] { wordList }));
			}
		}
		else if (description.startsWith("lc_complete(") && description.endsWith(")"))
        {      
	        //move up in the edge stack
            int ruleEdge = popEdge();
            
            //check whether we are in the recursive case
            if (currentEdge == -1)
            {
                //revert pop if we were at the topmost rule edge (called by lc)
                pushEdge(ruleEdge);
            }
            else
            {
                //not the topmost rule edge, we were called recursively by lc_complete
                setLastSpanEdge(ruleEdge);
    		    //the topmost rule edge is complete, we can cut its length to the current position
                state.getChart().setEdgeStatus(ruleEdge, 0);
                trimEdgeToChildrenLength(ruleEdge);
            }
        }
		else if (description.startsWith("db_word("))
	    {
	        //create chart edge with label "lex:word" from the current position to the next
	        String word = description.substring(8,description.indexOf(','));
	        int edgeID = state.getChart().addEdge(currentPosition, currentPosition + 1, "lex:" + word, 2);
	        state.linkEdgeToNode(edgeID, currentID);
	        if (lexEntryExistenceMode)
            {
	            state.getChart().setSegmentCaption(currentPosition, word);
	            currentPosition++;
            }
	        else if (edgeExists())
	        {
	            int motherEdge = getTopEdge();
	            state.getChart().addEdgeDependency(motherEdge, edgeID);
	            state.getChart().setLeftBoundForEdge(edgeID, getPos(motherEdge));
	            state.getChart().setRightBoundForEdge(edgeID, getPos(motherEdge) + 1);
	            setPos(edgeID, getPos(motherEdge) + 1);
	            pushEdge(edgeID);
	        }
	        else
	        {
	            //a freely dangling lexical edge, this should not happen
	            System.err.println("WARNING: found a lexical edge outside of any expected context");
	        }
	        kahina.dispatchEvent(new KahinaChartUpdateEvent(edgeID));
	    }
		else if (description.startsWith("lexentry_existence"))
		{
		    if (!lexEntryExistenceMode)
		    {
		        lexEntryExistenceMode = true;
		        topLexEntryExistenceStep = currentID;
		    }
		} 
        else if (description.startsWith("unify"))
        {
            if (edgeExists())
            {
                int motherEdge = getTopEdge();
                int motherPos = getPos(motherEdge);
                int edgeID = state.getChart().addEdge(motherPos, motherPos + 1, "unify", 2);
                state.linkEdgeToNode(edgeID, currentID);
                state.getChart().addEdgeDependency(motherEdge, edgeID);
                setPos(edgeID, motherPos);
                pushEdge(edgeID);
                kahina.dispatchEvent(new KahinaChartUpdateEvent(edgeID));
            }
            else
            {
                //a freely dangling unify edge, this should not happen
                System.err.println("WARNING: found a unify edge outside of any expected context!");
            }

        }  
	}
	
	public void call(int extID)
	{
	    super.call(extID);
	       if (VERBOSE) System.err.println("QTypeBridge.call(" + extID + ")");
	    int stepID = convertStepID(extID);
        String description = state.get(stepID).getGoalDesc();
        if (description.equals("grammar:db_rule/4"))
        {
            if (edgeExists())
            {
                int motherEdge = getTopEdge();
                int startPos = getPos(motherEdge);
                int edgeID = state.getChart().addEdge(startPos, state.getChart().getSegmentsWithCaption().size(), "rule", 2);
                setPos(edgeID, startPos);
                state.getChart().addEdgeDependency(motherEdge, edgeID);
                state.linkEdgeToNode(edgeID, currentID);
                pushEdge(edgeID);
            }
            else
            {
                //a freely dangling rule edge, this should not happen
                System.err.println("WARNING: found a rule edge outside of any expected context");
            }
        }
        else if (description.startsWith("parser:unify"))
        {
            int motherEdge = getTopEdge();
            int edgeID = state.getEdgeForNode(stepID);
            int lastSpanEdge = findLastSpanEdge(currentID);
            //the category we unify with is the one associated with the last span edge
            if (VERBOSE) System.err.println("  last span edge: " + lastSpanEdge);
            int leftBound = state.getChart().getLeftBoundForEdge(lastSpanEdge);
            int rightBound = state.getChart().getRightBoundForEdge(lastSpanEdge);
            if (rightBound == leftBound)
            {
                rightBound++;
                state.getChart().setRightBoundForEdge(motherEdge, rightBound);
            }
            state.getChart().setLeftBoundForEdge(edgeID, leftBound);
            state.getChart().setRightBoundForEdge(edgeID, rightBound);
        }
	}
	
	public void redo(int extID)
	{
	    if (VERBOSE) System.err.println("QTypeBridge.redo(" + extID + ")");
	       
	    //save the old internalID and retrieve the corresponding edge (moving up if necessary)
	    //the retrieved edge is the new context edge for the subsequent chart operations
	    int oldStepID = convertStepID(extID);
	    int oldEdgeID = state.getEdgeForNode(oldStepID);
        int motherEdgeStep = oldStepID;
	    while (oldEdgeID == -1)
	    {
            motherEdgeStep = state.getSecondaryStepTree().getParent(motherEdgeStep);
            oldEdgeID = state.getEdgeForNode(motherEdgeStep);
            if (motherEdgeStep == -1)
            {
                System.err.println("WARNING: reached call tree root without finding an edge!");
                break;
            }
            else
            {
                if (VERBOSE) System.err.println("    motherEdgeStep = " + motherEdgeStep);
            }
	    }
        if (VERBOSE) System.err.println("  oldEdgeID = " + oldEdgeID);
        pushEdge(oldEdgeID);
        
	    //process the redo in terms of logic programming; this updates the step tree
	    super.redo(extID);
	    
	    //chart operations; most are like at the call port, except that edge contents are copied
	    //some step types (unify etc.) are never redone during the parse process
	    int newStepID = convertStepID(extID);
	    String description = state.get(newStepID).getGoalDesc();
	    System.err.println("  goal description: " + description);
	    if (description.equals("grammar:db_rule/4"))
	    {
	        if (edgeExists())
            {
	            //throw away the last rule edge, it is not relevant any longer
	            popEdge();
                int motherEdge = getTopEdge();
                int startPos = getPos(motherEdge);
                int edgeID = state.getChart().addEdge(startPos, state.getChart().getSegmentsWithCaption().size(), "rule", 2);
                setPos(edgeID, startPos);
                state.getChart().addEdgeDependency(motherEdge, edgeID);
                state.linkEdgeToNode(edgeID, currentID);
                pushEdge(edgeID);
                
                setLastSpanEdge(oldEdgeID);
            }
            else
            {
                //a freely dangling rule edge, this should not happen
                System.err.println("WARNING: found a rule edge outside of any expected context");
            }
	    }
	    else if (description.equals("grammar:db_word/4"))
	    {
	        if (edgeExists())
            {
	            //throw away the last word edge, it is not relevant any longer
	            popEdge();
                int motherEdge = getTopEdge();
                int startPos = getPos(motherEdge);
    	        //TODO: do not just copy the old edge label, but find out which category the new lex entry has
                String caption = state.getChart().getEdgeCaption(oldEdgeID);
                int leftBound = state.getChart().getLeftBoundForEdge(oldEdgeID);
                int rightBound = state.getChart().getRightBoundForEdge(oldEdgeID);
    	        int edgeID = state.getChart().addEdge(leftBound, rightBound, caption, 2);
    	        setPos(edgeID, startPos);
                state.getChart().addEdgeDependency(motherEdge, edgeID);
    	        state.linkEdgeToNode(edgeID, currentID);
                pushEdge(edgeID);
                
                setLastSpanEdge(oldEdgeID);
            }
            else
            {
	            //a freely dangling lexical edge, this should not happen
	            System.err.println("WARNING: lexical edge was redone outside of any expected context");
            }
	    }
	    else if (description.equals("parser:lc_complete/8"))
	    {
	        
	        //retrieve the first child edge of the last call
	        //this edge is associated with a unify or db_rule step directly under the lc_complete step
	        int childEdgeStepID = state.getStepTree().getChildren(oldStepID).get(0);
	        int childEdge = state.getEdgeForNode(childEdgeStepID);
	        
            //reset the current position of the mother edge to the start of the rule edge
            int motherEdge = getTopEdge();
            setPos(motherEdge, state.getChart().getLeftBoundForEdge(childEdge));
            
            setLastSpanEdge(oldEdgeID);
            
            //two steps above the last call in the search tree, we find the step with the lastSpanEdge to restore
            /*int lastSpanEdgeStepID = state.getStepTree().getParent(oldStepID);
            int lastSpanEdge = state.getEdgeForNode(lastSpanEdgeStepID);
            while (lastSpanEdge == -1)
            {
                lastSpanEdgeStepID = state.getStepTree().getParent(lastSpanEdgeStepID);
                lastSpanEdge = state.getEdgeForNode(lastSpanEdgeStepID);
            }
            setLastSpanEdge(lastSpanEdge);*/
	    }
	    else if (description.equals("parser:lc/5"))
        {
            if (VERBOSE) System.err.println("lc/5 is being redone!");
            //copy over the edge reference, so that the correct position in the chart is found
            if (oldEdgeID != -1)
            {
                state.linkEdgeToNode(oldEdgeID, newStepID);
            }
            
	        /*String caption = state.getChart().getEdgeCaption(oldEdgeID);
	        if (edgeExists())
            {
                int motherEdge = getTopEdge();
                //int startPos = state.getChart().getLeftBoundForEdge(motherEdge); 
                int startPos = getPos(motherEdge);
                int edgeID = state.getChart().addEdge(startPos, state.getChart().getSegmentsWithCaption().size(), caption, 2);
                setPos(edgeID, startPos);
                state.linkEdgeToNode(edgeID, newStepID);
                state.getChart().addEdgeDependency(motherEdge, edgeID);
                pushEdge(edgeID);
            }
            else
            {
                //a freely dangling parse edge, this only happens at the top
                int edgeID = state.getChart().addEdge(0, state.getChart().getSegmentsWithCaption().size(), caption, 2);
                setPos(edgeID, 0);
                state.linkEdgeToNode(edgeID, newStepID);
                pushEdge(edgeID);
                kahina.dispatchEvent(new KahinaChartUpdateEvent(edgeID));
            }*/
        }
        else if (description.equals("parser:check_link/2"))
        {
            System.err.println("check_link/2 is being redone!");
        }
	}
	
    public int virtualRedo(int stepID)
    {
        //System.err.println("QTypeBridge.virtualRedo(" + stepID + ")");
        int newStepID = super.virtualRedo(stepID);
        
        String description = state.get(newStepID).getGoalDesc();
        
        Integer edgeID = state.getEdgeForNode(stepID);
        if (edgeID != -1)
        {
            state.linkEdgeToNode(edgeID, newStepID);
            if (description.startsWith("db_word("))
            {
                setLastSpanEdge(edgeID);
            }
        }
        return newStepID;
    }
	
	@Override
	public void exit(int extID, boolean deterministic, String newDescription)
	{		
		super.exit(extID, deterministic, newDescription);
		int stepID = convertStepID(extID);
		
	    if (VERBOSE) System.err.println("QTypeBridge.exit(" + extID + "," + deterministic + "," + newDescription + ")");
		
		// At exit of compile_grammar steps, register the grammar:
		if (newDescription.startsWith("compile_grammar(") && newDescription.endsWith(")"))
		{
			String path = PrologUtil.atomLiteralToString(newDescription.substring(16, newDescription.length() - 1));
			kahina.dispatchEvent(new KahinaControlEvent(QTypeControlEventCommands.REGISTER_GRAMMAR, new Object[] { path }));
		}
		//exit lex entry existence mode if we have come back to the topmost such node
		else if (newDescription.startsWith("lexentry_existence"))
		{
		    if (topLexEntryExistenceStep == stepID)
		    {
		        lexEntryExistenceMode = false;
		        topLexEntryExistenceStep = -1;
		        currentPosition = 0;
		    }
		}
		//lc was successful, we move up in the edge stack again
		else if (newDescription.startsWith("lc("))
		{
		    if (edgeExists())
		    {
		        int childEdge = popEdge();
		        trimEdgeToChildrenLength(childEdge);
		        int motherEdge = getTopEdge();
		        //the next item in lc_list will be tried, we can move the pos accordingly
		        setPos(motherEdge, getPos(childEdge));
                kahina.dispatchEvent(new KahinaChartUpdateEvent(motherEdge));
		    }
		    else
		    {
		        System.err.println("WARNING: lc exited on an empty edge stack!");
		    }
		}
		//the right border position of a successful rule edge is transferred to the calling edge
        else if (newDescription.startsWith("db_rule("))
        {
            if (edgeExists())
            {
                int childEdge = state.getEdgeForNode(stepID);
                int motherEdge = getTopEdge();
                //we will now continue scanning in the mother edge, so we can move the pos accordingly
                setPos(motherEdge, getPos(childEdge));
            }
            else
            {
                System.err.println("WARNING: db_word exited on an empty edge stack!");
            }
        }
		//unify was successful, we move up in the edge stack again
        else if (newDescription.startsWith("unify("))
        {
            if (edgeExists())
            {
                int unifyEdge = popEdge();
                int motherEdge = getTopEdge();
                setPos(motherEdge, state.getChart().getRightBoundForEdge(unifyEdge));
                kahina.dispatchEvent(new KahinaChartUpdateEvent(unifyEdge));
            }
            else
            {
                System.err.println("WARNING: unify exited on an empty edge stack!");
            }
        }
		
		//if we have an associated edge, set it to successful (except rule edges)
		int associatedEdge = state.getEdgeForNode(stepID);
		if (associatedEdge != -1 && !newDescription.startsWith("db_rule"))
		{
		    state.getChart().setEdgeStatus(associatedEdge, 0);
		}
	}
	
    public void fail(int extID)
    {
        super.fail(extID);
        int stepID = convertStepID(extID);
        
        //lc is done, we move up in the edge stack again
        if (state.get(stepID).getGoalDesc().equals("parser:lc/5"))
        {
            if (edgeExists())
            {
                int childEdge = popEdge();
                trimEdgeToChildrenLength(childEdge);
                //in this case, we do NOT move the mother edge pos
            }
            else
            {
                System.err.println("WARNING: lc failed on an empty edge stack!");
            }
        }
        //failed unification in a rule context determines the failure of the rule
        else if (state.get(stepID).getGoalDesc().equals("parser:unify/2"))
        {
            int ruleEdge = state.getEdgeForNode(findRuleParent(stepID));
            if (ruleEdge != -1)
            {
	            trimEdgeToChildrenLength(ruleEdge);
	            if (VERBOSE) System.err.println("Rule edge #" + ruleEdge + " failed.");
	            if (state.getChart().getEdgeStatus(ruleEdge) != 0)
	            {
	            	state.getChart().setEdgeStatus(ruleEdge, 1);
	            }
            }
            if (edgeExists())
            {
                int unifyEdge = popEdge();
                setPos(unifyEdge, state.getChart().getRightBoundForEdge(unifyEdge));
            }
            else
            {
                System.err.println("WARNING: unify failed on an empty edge stack!");
            }
        }
        //failed lc_list in a rule context determines the failure of the rule
        else if (state.get(stepID).getGoalDesc().equals("parser:lc_list/5"))
        {
            int ruleEdge = state.getEdgeForNode(findRuleParent(currentID));
            if (ruleEdge != -1)
            {
	            trimEdgeToChildrenLength(ruleEdge);
	            if (VERBOSE) System.err.println("Rule edge #" + ruleEdge + " failed.");
	            if (state.getChart().getEdgeStatus(ruleEdge) != 0)
	            {
	            	state.getChart().setEdgeStatus(ruleEdge, 1);
	            }
            }
        }
        else
        {
            if (VERBOSE) System.err.println("  untreated failed goal with description " + state.get(stepID).getGoalDesc() + "!");
        }
        
        //if we have an associated edge, set it to failure
        int associatedEdge = state.getEdgeForNode(stepID);
        if (associatedEdge != -1)
        {
            if (state.getChart().getEdgeStatus(associatedEdge) != 0)
            {
                state.getChart().setEdgeStatus(associatedEdge, 1);
            }
            kahina.dispatchEvent(new KahinaChartUpdateEvent(associatedEdge));
        }
    }

	public void registerExample(int number, String expectation, String sentence)
	{
		if (VERBOSE)
		{
			System.err.println(this + ".registerExample(" + number + ", " + expectation + ", " + sentence + ")");
		}
		try
		{
			kahina.dispatchEvent(new KahinaControlEvent(QTypeControlEventCommands.REGISTER_EXAMPLE, new Object[] { number, expectation, PrologUtil.parsePrologStringList(sentence) }));
		}
		catch (Exception e)
		{
			e.printStackTrace();
			System.exit(-1);
		}
	}

	@Override
	protected QTypeStep generateStep()
	{
		return new QTypeStep();
	}

	public void registerGoal(int extID, String key, String grisu)
	{
	    int stepID = stepIDConv.get(extID);
		try
		{
			QTypeGoal goal = state.retrieve(QTypeStep.class, stepID).getGoal();

			if ("fs".equals(key))
			{
				goal.setFS(packer.pack(grisu));
			} 
			else if ("tree".equals(key))
			{
				goal.setTree(packer.pack(grisu));
			} 
			else if ("in".equals(key))
			{
				goal.setIn(packer.pack(grisu));
				
                IEntity graleFS = GraleJUtility.grisuToGralej(grisu);
                
                //System.err.println("adding in FS of type " + GraleJUtility.getType(graleFS));
                if (GraleJUtility.getType(graleFS).equals("lc"))
                //lc: (update current position using the length of the unconsumed token list)
                //    add an edge representing the current subparse attempt
                {
                    List<String> path = new LinkedList<String>();
                    /*path.add("arg3");
                    IEntity argFS = GraleJUtility.delta(graleFS, path);
                    if (argFS == null || !(argFS instanceof IList))
                    {
                        System.err.println("WARNING: could not read current position from lc_complete argument!");
                    }
                    else
                    {
                        int listLength = GraleJUtility.listLength((IList) argFS);
                        currentPosition = state.getChart().getSegmentsWithCaption().size() - listLength;
                    }
                    path.clear();*/
                    path.add("arg2");
                    IEntity argFS = GraleJUtility.delta(graleFS, path);
                    if (argFS == null)
                    {
                        System.err.println("WARNING: could not read category from lc argument!");
                    }
                    else
                    {
                        String category = GraleJUtility.getType(argFS);
                        if (edgeExists())
                        {
                            int motherEdge = getTopEdge();
                            int rightBound = state.getChart().getSegmentsWithCaption().size();
                            if (getPos(motherEdge) == rightBound)
                        	{
                        		rightBound++;
                        		state.getChart().setRightBoundForEdge(motherEdge, rightBound);
                        	}
                            int edgeID = state.getChart().addEdge(getPos(motherEdge), rightBound, "parse " + category, 2);
                            setPos(edgeID, getPos(motherEdge));
                            state.linkEdgeToNode(edgeID, stepID);
                            state.getChart().addEdgeDependency(getTopEdge(), edgeID);
                            pushEdge(edgeID);
                        }
                        else
                        {
                            //a freely dangling parse edge, this only happens at the top
                            int edgeID = state.getChart().addEdge(0, state.getChart().getSegmentsWithCaption().size(), "parse " + category, 2);
                            setPos(edgeID, 0);
                            state.linkEdgeToNode(edgeID, stepID);
                            pushEdge(edgeID);
                            kahina.dispatchEvent(new KahinaChartUpdateEvent(edgeID));
                        }
                    }
                }
                if (GraleJUtility.getType(graleFS).equals("unify"))
                {
                    int associatedEdge = state.getEdgeForNode(stepID);
                    String newUnifyLabel = "~";
                    List<String> path = new LinkedList<String>();
                    /*path.add("arg3");
                    IEntity argFS = GraleJUtility.delta(graleFS, path);
                    if (argFS == null || !(argFS instanceof IList))
                    {
                        System.err.println("WARNING: could not read current position from lc_complete argument!");
                    }
                    else
                    {
                        int listLength = GraleJUtility.listLength((IList) argFS);
                        currentPosition = state.getChart().getSegmentsWithCaption().size() - listLength;
                    }
                    path.clear();*/
                    path.add("arg0");
                    IEntity arg0FS = GraleJUtility.delta(graleFS, path);
                    if (arg0FS == null)
                    {
                        System.err.println("WARNING: could not read category from first unify argument!");
                    }
                    else
                    {
                        while (arg0FS instanceof ITag)
                        {
                            arg0FS = ((ITag) arg0FS).target();
                        }
                        String category = GraleJUtility.getType(arg0FS);
                        newUnifyLabel = newUnifyLabel + category;
                    }
                    path.clear();
                    path.add("arg1");
                    IEntity arg1FS = GraleJUtility.delta(graleFS, path);
                    if (arg1FS == null)
                    {
                        System.err.println("WARNING: could not read category from second unify argument!");
                    }
                    else
                    {
                        while (arg1FS instanceof ITag)
                        {
                            arg1FS = ((ITag) arg1FS).target();
                        }
                        String category = GraleJUtility.getType(arg1FS);
                        newUnifyLabel = category + newUnifyLabel;
                    }
                    state.getChart().setEdgeCaption(associatedEdge, newUnifyLabel);
                    kahina.dispatchEvent(new KahinaChartUpdateEvent(associatedEdge));
                }        
			} 
			else if ("out".equals(key))
			{	    
				goal.setOut(packer.pack(grisu));
				
				IEntity graleFS = GraleJUtility.grisuToGralej(grisu);
				//read out useful info for edge labels from feature structures
				//if we have an associated edge...
		        int associatedEdge = state.getEdgeForNode(stepID);
		        if (associatedEdge != -1)
		        {
                    //...analyse the preliminary label to decide what to extract
		            String oldCaption = state.getChart().getEdgeCaption(associatedEdge);
		            if (oldCaption.startsWith("lex:"))
		            {
		                List<String> path = new LinkedList<String>();
		                path.add("arg1");
		                IEntity argFS = GraleJUtility.delta(graleFS, path);
		                if (argFS == null)
		                {
		                    System.err.println("WARNING: could not read determine category for lexical edge " + associatedEdge);
		                    state.getChart().setEdgeCaption(associatedEdge, "?" + oldCaption.substring(3));
		                }
		                else
		                {
		                    String type = GraleJUtility.getType(argFS);
		                    state.getChart().setEdgeCaption(associatedEdge, type + oldCaption.substring(3));
		                }
		            }
		            else if (oldCaption.equals("rule"))
		            {
		                //TODO: assign the edge to the lc_complete step above it!
		                List<String> path = new LinkedList<String>();
                        path.add("arg0");
                        IEntity argFS = GraleJUtility.delta(graleFS, path);
                        String newLabel = "";
                        if (argFS == null)
                        {
                            System.err.println("WARNING: could not read head category for rule edge " + associatedEdge);
                            newLabel = "? -> ";
                        }
                        else
                        {
                            String type = GraleJUtility.getType(argFS);
                            newLabel = type + " -> ";
                        }
                        path.clear();
                        path.add("arg1");
                        argFS = GraleJUtility.delta(graleFS, path);
                        if (argFS == null || !(argFS instanceof IList))
                        {
                            System.err.println("WARNING: could not read body categories for rule edge " + associatedEdge);
                            newLabel += "?";
                        }
                        else
                        {
                            for (IEntity catFS : ((IList) argFS).elements())
                            {
                                while (catFS instanceof ITag)
                                {
                                    catFS = ((ITag) catFS).target();
                                }
                                newLabel += GraleJUtility.getType(catFS) + " ";
                            }
                        }
                        state.getChart().setEdgeCaption(associatedEdge, newLabel);
		                kahina.dispatchEvent(new KahinaChartUpdateEvent(associatedEdge));
		            }
		        }
			}
		} 
		catch (Exception e)
		{
			e.printStackTrace();
			System.exit(-1);
		}
	}
	
	private void pushEdge(int edgeID)
	{
	    if (VERBOSE) System.err.println("  pushing " + edgeID + " on edge stack");
	    currentEdge = edgeID;
	}
	
    private int popEdge()
    {
        int poppedEdge = currentEdge;
        if (VERBOSE) System.err.println("  popping " + poppedEdge + " from edge stack");
        Set<Integer> motherEdges = state.getChart().getMotherEdgesForEdge(currentEdge);
        if (motherEdges.size() > 0)
        {
            //in the QType case, there is only one mother for each edge!
            currentEdge = motherEdges.iterator().next();
        }
        else
        {
            currentEdge = -1;
        }
        return poppedEdge;
    }
    
    private int getTopEdge()
    {
        if (VERBOSE) System.err.println("  getting top edge: " + currentEdge);
        return currentEdge;
    }
    
    private boolean edgeExists()
    {
        if (currentEdge == -1)
        {
            if (VERBOSE) System.err.println("  virtual edge stack is empty!");
            return false;
        }
        return true;
    }
    
    private int getPos(int edgeID)
    {
        return edgeToCurrentPosition.get(edgeID);
    }
    
    private void setPos(int edgeID, int pos)
    {
        if (VERBOSE) System.err.println("  setting pos of edge #" + edgeID + " to " + pos);
        edgeToCurrentPosition.put(edgeID, pos);
    }

    private void setLastSpanEdge(int lastSpanEdge)
    {
        //if (VERBOSE) 
        	System.err.println("  setLastSpanEdge(" + lastSpanEdge + "), nothing was stored");
    }
    
    //assumes that there is unify at stepID, whose span we need to determine
    private int findLastSpanEdge(int stepID)
    {
        //twice up in the call structure, latest search tree child will be db_word or db_rule, get associated edge
        int parentComplete = state.getSecondaryStepTree().getParent(stepID);
        System.err.println("parentComplete = " + parentComplete);
        int grandparentLcOrComplete = state.getSecondaryStepTree().getParent(parentComplete);
        System.err.println("grandparentLcOrComplete = " + grandparentLcOrComplete);
        List<Integer> children = state.getStepTree().getChildren(grandparentLcOrComplete);
        int lastSpanStep = children.get(children.size() - 1);
        System.err.println("lastSpanStep = " + lastSpanStep);
        Integer lastSpanEdge = state.getEdgeForNode(lastSpanStep);
        if (lastSpanEdge == null)
        {
            System.err.println("ERROR: failed to determine last span edge via "
                               + stepID + "/" + parentComplete + "/" + grandparentLcOrComplete + "\\" + lastSpanStep);
        }
        return lastSpanEdge;
    }
    
    //returns -1 if the rule parent is above an lc or lc_complete step
    private int findRuleParent(int stepID)
    {
    	//TODO: determine contextual information here (must not jump over lc and lc_complete)
        int ruleParentID = state.getStepTree().getParent(stepID);
        while (!state.getStepTree().getNodeCaption(ruleParentID).contains("db_rule"))
        {
            ruleParentID = state.getStepTree().getParent(ruleParentID);
            if (ruleParentID == -1)
            {
                System.err.println("WARNING: no rule parent found for step #" + stepID);
                return -1;
            }
            if (state.getStepTree().getNodeCaption(ruleParentID).contains("lc("))
            {
            	return -1;
            }
        }
        return ruleParentID;
    }
    
    private void trimEdgeToChildrenLength(int edgeID)
    {
        //shorten the edge length to the maximum length of any child
        int maxChildRightBound = state.getChart().getLeftBoundForEdge(edgeID);
        for (int childEdge : state.getChart().getDaughterEdgesForEdge(edgeID))
        {
            int childRightBound = state.getChart().getRightBoundForEdge(childEdge);
            if (childRightBound > maxChildRightBound)
            {
                maxChildRightBound = childRightBound;
            }
        }
        if (maxChildRightBound == state.getChart().getLeftBoundForEdge(edgeID)) maxChildRightBound++;
        state.getChart().setRightBoundForEdge(edgeID, maxChildRightBound);
    }
}
