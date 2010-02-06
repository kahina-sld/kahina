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
    
    //define the relation for this match (0 is reserved for (token) identity, i.e. Java ==)
    int rel = 0;
    
    //used for the most simple matchings (e.g. for all kinds of discrete values)
    int intValue = 0;
    
    //used for string matchings
    String stringValue;
    
    //precompiled regular expression, only used in case of String matching
    Pattern regexValue;
    
    //child pattern for complex boolean matches
    TreeNodePattern leftArg;
    TreeNodePattern rightArg;
    
    //predefined atomic match types (inheriting classes can add their own types)
    public static final int CAPTION = 1;
    public static final int EDGE_LABEL = 2;
    public static final int STATUS = 3;
    public static final int ID = 4;
    
    //boolean operators for more complex matches
    public static final int NEGATION = -1;
    public static final int CONJUNCTION = -2;
    public static final int DISJUNCTION = -3;
    public static final int IMPLICATION = -4;
    
    //predefined integer relations
    public static final int IDENTITY = 0;
    public static final int LESS = -2;
    public static final int GREATER = +2;
    public static final int LESS_OR_EQUAL = -1;
    public static final int GREATER_OR_EQUAL = +1;
    
    //predefined String relations
    public static final int EQUALITY = 3;
    public static final int MATCHING = 4;
    public static final int STARTS_WITH = 5;
    public static final int CONTAINS = 6;
    public static final int ENDS_WITH = 7;
    
    public TreeNodePattern()
    {
        this.type = 0;
        this.rel = 0;
        this.intValue = 0;
        this.stringValue = "";
        this.regexValue = Pattern.compile("");
        this.leftArg = null;
        this.rightArg = null;
    }
    
    public TreeNodePattern(int type, int rel, int value)
    {
        this.type = type;
        this.rel = rel;
        this.intValue = value;
        this.stringValue = "";
        this.regexValue = Pattern.compile("");
        this.leftArg = null;
        this.rightArg = null;
    }
    
    public TreeNodePattern(int type, int rel, String value)
    {
        this.type = type;
        this.rel = rel;
        this.intValue = 0;
        this.stringValue = value;
        this.regexValue = Pattern.compile(value);
        this.leftArg = null;
        this.rightArg = null;
    }
    
    public TreeNodePattern(int type, TreeNodePattern leftArg)
    {
        this.type = type;
        this.rel = 0;
        this.intValue = 0;
        this.stringValue = "";
        this.regexValue = Pattern.compile("");
        this.leftArg = leftArg;
        this.rightArg = null;
    }
    
    public TreeNodePattern(int type, TreeNodePattern leftArg, TreeNodePattern rightArg)
    {
        this.type = type;
        this.rel = 0;
        this.intValue = 0;
        this.stringValue = "";
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
    
    //TODO: adapt this to the new relation types, possibly with a more complex hierarchical decision function
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
            case CAPTION:
            {
                Matcher matcher = regexValue.matcher(m.getNodeCaption(nodeID));
                return matcher.matches();
            }
            case EDGE_LABEL:
            {
                Matcher matcher = regexValue.matcher(m.getNodeCaption(nodeID));
                return matcher.matches();
            }
            case STATUS:
            {
                return m.getNodeStatus(nodeID) == intValue;
            }
            case ID:
            {
                return nodeID == intValue;
            }
        }
        return true;
    }
    
    public String toString()
    {
        StringBuilder str = new StringBuilder();
        switch (type)
        {
            case NEGATION:
            {
                str.append("!(" + leftArg.toString() + ")");
                break;
            }
            case CONJUNCTION:
            {
                str.append("(" + leftArg.toString() + " & " + rightArg.toString() + ")");
                break;
            }
            case DISJUNCTION:
            {
                str.append("(" + leftArg.toString() + " V " + rightArg.toString() + ")");
                break;
            }
            case IMPLICATION:
            {
                str.append("(" + leftArg.toString() + " -> " + rightArg.toString() + ")");
                break;
            }
            default:
            {
                str.append(getTypeString() + getValueString());
            }
        }
        return str.toString();
    }
    
    /**
     * defines a very short symbolic representation of the type for an atomic pattern<br>
     * is the empty string for complex nodes or non-standard types<br>
     * inheriting classes should reimplement this if they add more types, relying on this version for the standard types
     * @return a short symbolic representation of the type
     */
    public String getTypeString()
    {
        switch (type)
        {
            case CAPTION: return "ca";
            case EDGE_LABEL: return "ed";
            case STATUS: return "st";
            case ID: return "id";          
        }
        return "";
    }
    
    /**
     * defines a representation of the relation and the value<br>
     * is the empty string for complex nodes or non-standard relations<br>
     * inheriting classes should reimplement this if they add more relations or value types, relying on this version for the standard types
     * @return a representation of the relation and the value
     */
    public String getValueString()
    {
        switch (rel)
        {
            case IDENTITY: return "=" + intValue;
            case LESS: return "<" + intValue;
            case LESS_OR_EQUAL: return "<=" + intValue;
            case GREATER: return ">" + intValue;
            case GREATER_OR_EQUAL: return ">=" + intValue; 
            case EQUALITY: return "=" + stringValue; 
            case MATCHING: return "~=" + stringValue; 
            case STARTS_WITH: return ".startsWith(" + stringValue + ")"; 
            case CONTAINS: return ".contains(" + stringValue + ")"; 
            case ENDS_WITH: return ".endsWith(" + stringValue + ")"; 
        }
        return "";
    }
}
