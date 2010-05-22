package org.kahina.core.breakpoint;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.io.color.ColorIO;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

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
    
    public void parseValue(String value) throws PatternFormatException
    {
        if (type == ID)
        {
            try
            {
                this.intValue = Integer.parseInt(value);
            }
            catch (NumberFormatException e)
            {
                throw new PatternFormatException("Integer", value);
            }
        }
        else
        {
            if (rel == MATCHING)
            {
                try
                {
                    this.regexValue = Pattern.compile(value);
                    this.stringValue = value;
                }
                catch (PatternSyntaxException e)
                {
                    throw new PatternFormatException("RegEx", value);
                }
            }
            else
            {
                this.stringValue = value;
            }
        }
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
    
    public int getRel()
    {
        return rel;
    }

    public void setRel(int rel)
    {
        this.rel = rel;
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
                switch (rel)
                {
                    case EQUALITY: return m.getNodeCaption(nodeID).equals(stringValue);
                    case MATCHING:
                    {
                        Matcher matcher = regexValue.matcher(m.getNodeCaption(nodeID));
                        return matcher.matches();
                    }
                    case STARTS_WITH: return m.getNodeCaption(nodeID).startsWith(stringValue);
                    case CONTAINS: return m.getNodeCaption(nodeID).contains(stringValue);
                    case ENDS_WITH: return m.getNodeCaption(nodeID).endsWith(stringValue);
                    default: return false;
                }
            }
            case EDGE_LABEL:
            {
                switch (rel)
                {
                    case EQUALITY: return m.getEdgeLabel(nodeID).equals(stringValue);
                    case MATCHING:
                    {
                        Matcher matcher = regexValue.matcher(m.getEdgeLabel(nodeID));
                        return matcher.matches();
                    }
                    case STARTS_WITH: return m.getEdgeLabel(nodeID).startsWith(stringValue);
                    case CONTAINS: return m.getEdgeLabel(nodeID).contains(stringValue);
                    case ENDS_WITH: return m.getEdgeLabel(nodeID).endsWith(stringValue);
                    default: return false;
                }
            }
            case STATUS:
            {
                return m.getNodeStatus(nodeID) == intValue;
            }
            case ID:
            {
                switch (rel)
                {
                    case IDENTITY: return nodeID == intValue;
                    case LESS: return nodeID < intValue;
                    case GREATER: return nodeID > intValue;
                    case LESS_OR_EQUAL: return nodeID <= intValue;
                    case GREATER_OR_EQUAL: return nodeID >= intValue;
                    default: return false;
                }
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
    
    public String getTypeAsString()
    {
        switch (type)
        {
            case CAPTION: return "step label";
            case EDGE_LABEL: return "step origin";
            case STATUS: return "step id";
            case ID: return "step type";          
        }
        return "--";
    }
    
    public String getRelAsString()
    {
        switch (rel)
        {
            case IDENTITY: return "=";
            case LESS: return "<";
            case LESS_OR_EQUAL: return "<=";
            case GREATER: return ">";
            case GREATER_OR_EQUAL: return ">="; 
            case EQUALITY: return "equals"; 
            case MATCHING: return "matches"; 
            case STARTS_WITH: return "starts with"; 
            case CONTAINS: return "contains"; 
            case ENDS_WITH: return "ends with"; 
        }    
        return "";
    }
    
    public String getValueAsString()
    {
        switch (type)
        {
            case CAPTION: return stringValue;
            case EDGE_LABEL: return stringValue;
            case STATUS: return intValue + "";
            case ID: return "--";          
        }
        return "--";
    }
    
    public void setType(String typeString)
    {
        if (typeString.equals("step label"))
        {
            type = CAPTION;
        }
        else if (typeString.equals("step origin"))
        {
            type = EDGE_LABEL;
        }
        else if (typeString.equals("step id"))
        {
            type = ID;
        }
        else if (typeString.equals("step type"))
        {
            type = STATUS;
        }
        else
        {
            type = 0;
        }
    }
    
    public void setXMLType(String typeString)
    {
        if (typeString.equals("neg"))
        {
            type = NEGATION;
        }
        else if (typeString.equals("conj"))
        {
            type = CONJUNCTION;
        }
        else if (typeString.equals("disj"))
        {
            type = DISJUNCTION;
        }
        else if (typeString.equals("impl"))
        {
            type = IMPLICATION;
        }
        else if (typeString.equals("caption"))
        {
            type = CAPTION;
        }
        else if (typeString.equals("edgeLabel"))
        {
            type = EDGE_LABEL;
        }
        else if (typeString.equals("status"))
        {
            type = STATUS;
        }
        else if (typeString.equals("id"))
        {
            type = ID;
        }
        else
        {
            type = 0;
        }
    }
    
    public void setRelation(String relString)
    {
        if (relString.equals("="))
        {
            rel = IDENTITY;
        }
        else if (relString.equals(">"))
        {
            rel = GREATER;
        }
        else if (relString.equals("<"))
        {
            rel = LESS;
        }
        else if (relString.equals(">="))
        {
            rel = GREATER_OR_EQUAL;
        }
        else if (relString.equals("<="))
        {
            rel = LESS_OR_EQUAL;
        }
        else if (relString.equals("equals"))
        {
            rel = EQUALITY;
        }
        else if (relString.equals("matches"))
        {
            rel = MATCHING;
        }
        else if (relString.equals("starts with"))
        {
            rel = STARTS_WITH;
        }
        else if (relString.equals("contains"))
        {
            rel = CONTAINS;
        }
        else if (relString.equals("ends with"))
        {
            rel = ENDS_WITH;
        }
        else
        {
            rel = 0;
        }
    }
    
    public void setXMLRelation(String relString)
    {
        if (relString.equals("id"))
        {
            rel = IDENTITY;
        }
        else if (relString.equals("gt"))
        {
            rel = GREATER;
        }
        else if (relString.equals("lt"))
        {
            rel = LESS;
        }
        else if (relString.equals("geq"))
        {
            rel = GREATER_OR_EQUAL;
        }
        else if (relString.equals("leq"))
        {
            rel = LESS_OR_EQUAL;
        }
        else if (relString.equals("eq"))
        {
            rel = EQUALITY;
        }
        else if (relString.equals("match"))
        {
            rel = MATCHING;
        }
        else if (relString.equals("startsWith"))
        {
            rel = STARTS_WITH;
        }
        else if (relString.equals("contains"))
        {
            rel = CONTAINS;
        }
        else if (relString.equals("endsWith"))
        {
            rel = ENDS_WITH;
        }
        else
        {
            rel = 0;
        }
    }
    
    public String getTypeAsXMLString()
    {
        switch (type)
        {
            case NEGATION: return "neg";
            case CONJUNCTION: return "conj";
            case DISJUNCTION: return "disj";
            case IMPLICATION: return "impl";    
            case CAPTION: return "caption";
            case EDGE_LABEL: return "edgeLabel";
            case STATUS: return "status";
            case ID: return "id";          
        }
        return "";
    }
    
    public String getRelAsXMLString()
    {
        switch (rel)
        {
            case IDENTITY: return "id";
            case LESS: return "lt";
            case LESS_OR_EQUAL: return "leq";
            case GREATER: return "gt";
            case GREATER_OR_EQUAL: return "geq"; 
            case EQUALITY: return "eq"; 
            case MATCHING: return "match"; 
            case STARTS_WITH: return "startsWith"; 
            case CONTAINS: return "contains"; 
            case ENDS_WITH: return "endsWith"; 
        }
        return "";
    }
    
    public String exportXML(boolean asFile)
    {
        StringBuilder b = new StringBuilder("");
        if (asFile) b.append("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");
        b.append("<pattern type=\"" + getTypeAsXMLString() + "\" rel=\"" + getRelAsXMLString() + "\">\n");
        if (intValue != -1)
        {
            b.append("<intVal>" + intValue + "</intVal>");
        }
        if (stringValue.length() > 0 && !stringValue.equals("--"))
        {
            b.append("<stringVal regex=\"" +  (regexValue != null) + "\">" + stringValue + "</stringVal>");
        }
        if (leftArg != null)
        {
            b.append("<leftArg>\n");
            b.append(leftArg.exportXML(false));
            b.append("</leftArg>\n");
        }
        if (rightArg != null)
        {
            b.append("<rightArg>\n");
            b.append(rightArg.exportXML(false));
            b.append("</rightArg>\n");
        }
        b.append("</pattern>");
        return b.toString();
    }
    
    public static TreeNodePattern importXML(Element treeNodePatternNode)
    {
        //find the "pattern" child node, this will be the root element for the pattern
        for (int i = 0; i < treeNodePatternNode.getChildNodes().getLength(); i++)
        {
            //System.err.println("   Child node of name: " + childNodes.item(i).getNodeName());
            if (treeNodePatternNode.getChildNodes().item(i).getNodeName().equals("pattern"))
            {
                treeNodePatternNode = (Element) treeNodePatternNode.getChildNodes().item(i);
            }
        }
        TreeNodePattern newTreeNodePattern = new TreeNodePattern();
        newTreeNodePattern.setXMLType(treeNodePatternNode.getAttribute("type"));
        newTreeNodePattern.setXMLRelation(treeNodePatternNode.getAttribute("rel"));  
        NodeList childNodes = treeNodePatternNode.getChildNodes();
        for (int i = 0; i < childNodes.getLength(); i++)
        {
            String nodeName = childNodes.item(i).getNodeName();
            if (nodeName.equals("intVal"))
            {
                String intValString = childNodes.item(i).getTextContent();
                newTreeNodePattern.intValue = Integer.parseInt(intValString);
            }
            else if (nodeName.equals("stringVal"))
            {
                newTreeNodePattern.stringValue = childNodes.item(i).getTextContent();
                if (((Element) (childNodes.item(i))).getAttribute("regEx").equals(true))
                {
                    newTreeNodePattern.regexValue = Pattern.compile(newTreeNodePattern.stringValue);
                }
            }
            else if (nodeName.equals("leftArg"))
            {
                newTreeNodePattern.leftArg = TreeNodePattern.importXML((Element) childNodes.item(i));
            }
            else if (nodeName.equals("rightArg"))
            {
                newTreeNodePattern.rightArg = TreeNodePattern.importXML((Element) childNodes.item(i));
            }
        }
        return newTreeNodePattern;
    }
}
