package org.kahina.breakpoint;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.kahina.data.tree.KahinaTree;

/**
 * TreeNodePatterns are a means of defining which nodes are accepted
 * for use within TreeAutomatonRules as part of the Kahina breakpoint mechanism.
 * 
 * Some basic functionality is predefined, allowing the user to exploit
 * Kahina's predefined tree node properties such as 
 * 
 * For more complex breakpoint patterns based on associated step data, 
 * it is possible to use one of the specialized TreeNodePattern classes (such as SourceCodePattern) 
 * or to directly inherit from this class and overwrite or overload the decision methods.
 * 
 * It is not recommended to use this for child pattern matching, because this breaks the tree automaton logic.
 * For such patterns, use the TreeAutomaton mechanism of node annotations.
 * 
 * @author johannes
 *
 */
public class TreeNodePattern
{
    //define the type of the match (0 is reserved for "always matching")
    int type = 0;
    
    //used for the most simple matchings (e.g. for all kinds of discrete values)
    int intValue = 0;
    
    //precompiled regular expression, only used in case of String matching
    Pattern regexValue;
    
    //child pattern for complex boolean matches
    TreeNodePattern leftArg;
    TreeNodePattern rightArg;
    
    //predefined atomic match types (inheriting classes can add their own types)
    public static final int CAPTION_REGEX = 1;
    public static final int EDGE_LABEL_REGEX = 2;
    public static final int STATUS_MATCH = 3;
    public static final int ID_MATCH = 4;
    
    //boolean operators for more complex matches
    public static final int NEGATION = -1;
    public static final int CONJUNCTION = -2;
    public static final int DISJUNCTION = -3;
    public static final int IMPLICATION = -4;
    
    public TreeNodePattern()
    {
        this.type = 0;
        this.intValue = 0;
        this.regexValue = Pattern.compile("");
        this.leftArg = null;
        this.rightArg = null;
    }
    
    public TreeNodePattern(int type, int value)
    {
        this.type = type;
        this.intValue = value;
        this.regexValue = Pattern.compile("");
        this.leftArg = null;
        this.rightArg = null;
    }
    
    public TreeNodePattern(int type, String value)
    {
        this.type = type;
        this.intValue = 0;
        this.regexValue = Pattern.compile(value);
        this.leftArg = null;
        this.rightArg = null;
    }
    
    public TreeNodePattern(int type, TreeNodePattern leftArg)
    {
        this.type = type;
        this.intValue = 0;
        this.regexValue = Pattern.compile("");
        this.leftArg = leftArg;
        this.rightArg = null;
    }
    
    public TreeNodePattern(int type, TreeNodePattern leftArg, TreeNodePattern rightArg)
    {
        this.type = type;
        this.intValue = 0;
        this.regexValue = Pattern.compile("");
        this.leftArg = leftArg;
        this.rightArg = rightArg;
    }
    
    public int getType()
    {
        return type;
    }
    
    public void setType(int type)
    {
        this.type = type;
        adaptArgumentsToType();
    }
    
    private void adaptArgumentsToType()
    {
        //TODO: initialize or set to null leftArg and rightArg according to type
    }
    
    public void setLeftArgument(TreeNodePattern left)
    {
        this.leftArg = left;
    }
    
    public void setRightArgument(TreeNodePattern right)
    {
        this.rightArg = right;
    }
    
    public void switchArguments()
    {
        if (rightArg != null)
        {
            TreeNodePattern temp = leftArg;
            leftArg = rightArg;
            rightArg = temp;
        }
    }
    
    public TreeNodePattern getLeftArgument()
    {
        return leftArg;
    }
    
    public TreeNodePattern getRightArgument()
    {
        return rightArg;
    }
    
    public boolean matches(KahinaTree m, int nodeID)
    {
        switch (type)
        {
            case NEGATION:
            {
                return !leftArg.matches(m,nodeID);
            }
            case CONJUNCTION:
            {
                return leftArg.matches(m, nodeID) && rightArg.matches(m, nodeID);
            }
            case DISJUNCTION:
            {
                return leftArg.matches(m, nodeID) || rightArg.matches(m, nodeID); 
            }
            case IMPLICATION:
            {
                return !leftArg.matches(m,nodeID) || rightArg.matches(m, nodeID);
            }
            case CAPTION_REGEX:
            {
                Matcher matcher = regexValue.matcher(m.getNodeCaption(nodeID));
                return matcher.matches();
            }
            case EDGE_LABEL_REGEX:
            {
                Matcher matcher = regexValue.matcher(m.getNodeCaption(nodeID));
                return matcher.matches();
            }
            case STATUS_MATCH:
            {
                return m.getNodeStatus(nodeID) == intValue;
            }
            case ID_MATCH:
            {
                return nodeID == intValue;
            }
        }
        return true;
    }
}
