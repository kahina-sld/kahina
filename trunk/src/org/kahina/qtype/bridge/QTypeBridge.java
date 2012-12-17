package org.kahina.qtype.bridge;

import gralej.om.IEntity;
import gralej.om.IList;
import gralej.om.IRelation;
import gralej.om.ITag;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.kahina.core.control.KahinaControlEvent;
import org.kahina.core.control.KahinaStepUpdateEvent;
import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.gui.event.KahinaChartUpdateEvent;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.prolog.util.PrologUtil;
import org.kahina.qtype.QTypeDebuggerInstance;
import org.kahina.qtype.QTypeState;
import org.kahina.qtype.QTypeStep;
import org.kahina.qtype.control.QTypeControlEventCommands;
import org.kahina.qtype.data.bindings.QTypeGoal;
import org.kahina.sicstus.bridge.SICStusPrologBridge;
import org.kahina.tralesld.data.fs.TraleSLDFS;
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
	private List<Integer> edgeStack;
	Map<Integer,Integer> edgeToCurrentPosition;
	int lastRuleNode = -1;

	public QTypeBridge(final QTypeDebuggerInstance kahina)
	{
		super(kahina);
	    this.state = kahina.getState();
		packer = new TraleSLDFSPacker();
		edgeStack = new LinkedList<Integer>();
		edgeToCurrentPosition = new HashMap<Integer,Integer>();
	}

	@Override
	public void step(int extID, String type, String description, String consoleMessage)
	{
	    super.step(extID, type, description, consoleMessage);
	       
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
			
			//each lc nodes open a new context for rule nodes
	        lastRuleNode = -1;
			//TODO: otherwise, detect the prediction (???)
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
	        else if (edgeStack.size() > 0)
	        {
	            int motherEdge = edgeStack.get(0);
	            state.getChart().addEdgeDependency(motherEdge, edgeID);
	            setPos(edgeID, currentPosition++);
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
            if (edgeStack.size() > 0)
            {
                int motherEdge = edgeStack.get(0);
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
	    int stepID = convertStepID(extID);
        String description = state.get(stepID).getGoalDesc();
        if (description.equals("grammar:db_rule/4"))
        {
            if (edgeStack.size() > 0)
            {
                int motherEdge = edgeStack.get(0);
                int startPos = getPos(motherEdge);
                int edgeID = state.getChart().addEdge(startPos, state.getChart().getRightBound(), "rule", 2);
                setPos(edgeID, startPos);
                state.getChart().addEdgeDependency(motherEdge, edgeID);
                state.linkEdgeToNode(edgeID, currentID);
                pushEdge(edgeID);
                lastRuleNode = currentID;
            }
            else
            {
                //a freely dangling rule edge, this should not happen
                System.err.println("WARNING: found a rule edge outside of any expected context");
            }
        }
	}
	
	public void redo(int extID)
	{
	    super.redo(extID);
	    int stepID = convertStepID(extID);
	    String description = state.get(stepID).getGoalDesc();
	    if (description.equals("grammar:db_rule/4"))
	    {
	        if (edgeStack.size() > 0)
            {
	            //throw away the last rule edge, it is not relevant any longer
	            popEdge();
                int motherEdge = edgeStack.get(0);
                int startPos = getPos(motherEdge);
                int edgeID = state.getChart().addEdge(startPos, state.getChart().getRightBound(), "rule", 2);
                setPos(edgeID, startPos);
                state.getChart().addEdgeDependency(motherEdge, edgeID);
                state.linkEdgeToNode(edgeID, currentID);
                pushEdge(edgeID);
                lastRuleNode = currentID;
            }
            else
            {
                //a freely dangling rule edge, this should not happen
                System.err.println("WARNING: found a rule edge outside of any expected context");
            }
	    }
	}
	
	@Override
	public void exit(int extID, boolean deterministic, String newDescription)
	{		
		super.exit(extID, deterministic, newDescription);
		int stepID = convertStepID(extID);
		
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
		//lc_complete was successful, we move up in the edge stack again
		else if (newDescription.startsWith("lc("))
		{
		    if (edgeStack.size() > 0)
		    {
		        int childEdge = popEdge();
		        int motherEdge = edgeStack.get(0);
		        //the next item in lc_list will be tried, we can move the pos accordingly
		        setPos(motherEdge, getPos(childEdge));
		    }
		    else
		    {
		        System.err.println("WARNING: lc exited on an empty edge stack!");
		    }
		}
		//move the position of the mother after a successful lex edge
        /*else if (newDescription.startsWith("db_word("))
        {
            if (edgeStack.size() > 0)
            {
                int childEdge = state.getEdgeForNode(stepID);
                int motherEdge = edgeStack.get(0);
                //we will now continue scanning in the mother edge, so we can move the pos accordingly
                setPos(motherEdge, getPos(childEdge));
            }
            else
            {
                System.err.println("WARNING: db_word exited on an empty edge stack!");
            }
        }*/
		//lc_complete was successful, we move up in the edge stack again
        else if (newDescription.startsWith("unify("))
        {
            if (edgeStack.size() > 0)
            {
                int unifyEdge = popEdge();
                int motherEdge = edgeStack.get(0);
                setPos(motherEdge, state.getChart().getRightBoundForEdge(unifyEdge));
            }
            else
            {
                System.err.println("WARNING: unify exited on an empty edge stack!");
            }
        }
		//successful unification in a rule context determines the success of the rule
        /*else if (newDescription.startsWith("unify("))
        {
            int ruleEdge = state.getEdgeForNode(lastRuleNode);
            state.getChart().setEdgeStatus(ruleEdge, 0);
            kahina.dispatchEvent(new KahinaChartUpdateEvent(ruleEdge));
        }*/
        
		
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
        
        //lc_complete is done, we move up in the edge stack again
        if (state.get(stepID).getGoalDesc().equals("parser:lc/5"))
        {
            if (edgeStack.size() > 0)
            {
                popEdge();
            }
            else
            {
                System.err.println("WARNING: lc failed on an empty edge stack!");
            }
        }
        //failed unification in a rule context determines the failure of the rule
        else if (state.get(stepID).getGoalDesc().equals("parser:unify/2"))
        {
            int ruleEdge = state.getEdgeForNode(lastRuleNode);
            System.err.println("Rule edge #" + ruleEdge + " failed.");
            state.getChart().setEdgeStatus(ruleEdge, 1);
            if (edgeStack.size() > 0)
            {
                popEdge();
            }
            else
            {
                System.err.println("WARNING: unify failed on an empty edge stack!");
            }
        }
        
        //if we have an associated edge, set it to failure
        int associatedEdge = state.getEdgeForNode(stepID);
        if (associatedEdge != -1)
        {
            state.getChart().setEdgeStatus(associatedEdge, 1);
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
                        currentPosition = state.getChart().getRightBound() - listLength;
                    }
                    path.clear();*/
                    path.add("arg2");
                    IEntity argFS = GraleJUtility.delta(graleFS, path);
                    if (argFS == null)
                    {
                        System.err.println("WARNING: could not read category from lc_complete argument!");
                    }
                    else
                    {
                        String category = GraleJUtility.getType(argFS);
                        if (edgeStack.size() > 0)
                        {
                            int motherEdge = edgeStack.get(0);
                            int edgeID = state.getChart().addEdge(getPos(motherEdge), state.getChart().getRightBound(), "parse " + category, 2);
                            setPos(edgeID, getPos(motherEdge));
                            state.linkEdgeToNode(edgeID, stepID);
                            state.getChart().addEdgeDependency(edgeStack.get(0), edgeID);
                            pushEdge(edgeID);
                        }
                        else
                        {
                            //a freely dangling parse edge, this only happens at the top
                            int edgeID = state.getChart().addEdge(0, state.getChart().getRightBound(), "parse " + category, 2);
                            setPos(edgeID, 0);
                            pushEdge(edgeID);
                            kahina.dispatchEvent(new KahinaChartUpdateEvent(edgeID));
                        }

                    }
                }
                if (GraleJUtility.getType(graleFS).equals("unify"))
                //lc: (update current position using the length of the unconsumed token list)
                //    add an edge representing the current subparse attempt
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
                        currentPosition = state.getChart().getRightBound() - listLength;
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
	    System.err.println("  pushing " + edgeID + " on edge stack");
	    edgeStack.add(0,edgeID);
	}
	
    private int popEdge()
    {
        int edgeID = edgeStack.remove(0);
        System.err.println("  popping " + edgeID + " from edge stack");
        return edgeID;
    }
    
    private int getPos(int edgeID)
    {
        return edgeToCurrentPosition.get(edgeID);
    }
    
    private void setPos(int edgeID, int pos)
    {
        System.err.println("  setting pos of edge #" + edgeID + " to " + pos);
        edgeToCurrentPosition.put(edgeID, pos);
    }
}
