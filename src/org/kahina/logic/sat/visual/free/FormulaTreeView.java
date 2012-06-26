package org.kahina.logic.sat.visual.free;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.KahinaInstance;
import org.kahina.core.data.tree.KahinaMemTree;
import org.kahina.core.visual.tree.KahinaTreeView;
import org.kahina.core.visual.tree.KahinaTreeViewPanel;
import org.kahina.logic.sat.data.free.BooleanConstant;
import org.kahina.logic.sat.data.free.BooleanFormula;
import org.kahina.logic.sat.data.free.BooleanVariable;
import org.kahina.logic.sat.data.free.Conjunction;
import org.kahina.logic.sat.data.free.Disjunction;
import org.kahina.logic.sat.data.free.Negation;

public class FormulaTreeView extends KahinaTreeView
{
    BooleanFormula formula;
    //map from subformulas to tree node IDs
    Map<BooleanFormula, Integer> frmToNode;
    //map from tree node IDs to subformulas
    public Map<Integer, BooleanFormula> nodeToFrm;
    
    boolean textDisplay;
    String displayText;
    
    public FormulaTreeView(KahinaInstance<?, ?, ?> kahina)
    {
        super(kahina);
        frmToNode = new HashMap<BooleanFormula, Integer>();
        nodeToFrm = new HashMap<Integer, BooleanFormula>();
        textDisplay = false;
    }
    
    public void displayFormula(BooleanFormula formula)
    {
        frmToNode.clear();
        nodeToFrm.clear();
        this.formula = formula;
        super.display(new KahinaMemTree());
        int rootID = model.addNode(generateNodeCaption(formula), "", 0);
        model.setRootID(rootID);
        frmToNode.put(formula, rootID);
        nodeToFrm.put(rootID, formula);
        recalculate();
        textDisplay = false;
    }
    
    //allows to display a text instead of a graph
    //TODO: consider adding this option to all views per default!
    public void displayText(String text)
    {
        textDisplay = true;
        displayText = text;
    }
    
    public void recursiveDecollapse(int nodeID)
    {
        formulaDecollapse(nodeID);
        for (int childID : model.getChildren(nodeID))
        {
            recursiveDecollapse(childID);
        }
    }
    
    public void formulaDecollapse(int nodeID)
    {
        model.setNodeStatus(nodeID, 1);
        BooleanFormula frm = nodeToFrm.get(nodeID);
        if (frm != null)
        {
            if (frm instanceof Negation)
            {
                frm = ((Negation) frm).getArg();
            }
            
            if (frm instanceof Conjunction)
            {
                Conjunction f = (Conjunction) frm;
                for (BooleanFormula subf : f.getFms())
                {
                    addFormulaNode(subf, nodeID);
                }
            }
            else if (frm instanceof Disjunction)
            {
                Disjunction f = (Disjunction) frm;
                for (BooleanFormula subf : f.getFms())
                {
                    addFormulaNode(subf, nodeID);
                }
            }
        }
    }
    
    public void formulaCollapse(int nodeID)
    {
        model.setNodeStatus(nodeID, 0);
        List<Integer> children = model.getChildren(nodeID);
        while (children.size() > 0)
        {
            removeNode(children.get(0));
        }
    }
    
    public void cycleDecollapsePrune(int nodeID)
    {
        if (model.getNodeStatus(nodeID) == 0)
        {
            formulaDecollapse(nodeID);
        }
        else if (model.getNodeStatus(nodeID) == 1)
        {
            formulaCollapse(nodeID);
            model.setNodeStatus(nodeID, 2);
            BooleanFormula frm = nodeToFrm.get(nodeID);
            if (frm != null)
            {
                frm.setPruned(true);
            }
        }
        else if (model.getNodeStatus(nodeID) == 2)
        {     
            model.setNodeStatus(nodeID, 0);
            BooleanFormula frm = nodeToFrm.get(nodeID);
            if (frm != null)
            {
                frm.setPruned(false);
            }
        }
    }
    
    private void addFormulaNode(BooleanFormula f, int parentID)
    {
        //System.err.println("addFormulaNode(" + f + "," + parentID + ")");
        int nodeID = model.addNode(generateNodeCaption(f), "", generateInitialStatus(f));
        frmToNode.put(f, nodeID);
        nodeToFrm.put(nodeID, f);
        model.addChild(parentID, nodeID);
    }
    
    private void removeNode(int nodeID)
    {
        List<Integer> children = model.getChildren(nodeID);
        while (children.size() > 0)
        {
            removeNode(children.get(0));
        }
        model.removeLeaf(nodeID);
        BooleanFormula frm = nodeToFrm.remove(nodeID);
        frmToNode.remove(frm);
    }

    private String generateNodeCaption(BooleanFormula f)
    {
        if (f instanceof BooleanConstant)
        {
            return ((BooleanConstant) f).toString();
        }
        else if (f instanceof BooleanVariable)
        {
            return ((BooleanVariable) f).toString();
        }
        else if (f instanceof Conjunction)
        {
            return "∧(" + ((Conjunction) f).getFms().size() + ")(" + f.getSize() + ")";
        }
        else if (f instanceof Disjunction)
        {
            return "v(" + ((Disjunction) f).getFms().size() + ")(" + f.getSize() + ")";
        }
        else if (f instanceof Negation)
        {
            return "¬" + generateNodeCaption(((Negation) f).getArg());
        }
        else
        {
            return "?";
        }
    }
    
    private int generateInitialStatus(BooleanFormula f)
    {
        if (formula.isPruned()) return 2;
        if (f instanceof Conjunction || f instanceof Disjunction)
        {
            return 0;
        }
        else if (f instanceof Negation)
        {
            return generateInitialStatus(((Negation) f).getArg());
        }
        else return 1;
    }
    
    @Override
    public JComponent makePanel()
    {
        KahinaTreeViewPanel panel = new FormulaTreeViewPanel(kahina);
        kahina.getGuiControl().registerListener("redraw", panel);
        panel.setView(this);
        return panel;
        /*JScrollPane scrollPane = new JScrollPane(panel);
        scrollPane.getViewport().setBackground(config.getBackgroundColor());
        return scrollPane;*/
    }

}
