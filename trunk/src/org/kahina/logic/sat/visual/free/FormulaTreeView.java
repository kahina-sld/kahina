package org.kahina.logic.sat.visual.free;

import java.util.HashMap;
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
    Map<Integer, BooleanFormula> nodeToFrm;
    
    public FormulaTreeView(KahinaInstance<?, ?, ?> kahina)
    {
        super(kahina);
        frmToNode = new HashMap<BooleanFormula, Integer>();
        nodeToFrm = new HashMap<Integer, BooleanFormula>();
    }
    
    public void displayFormula(BooleanFormula formula)
    {
        frmToNode.clear();
        nodeToFrm.clear();
        this.formula = formula;
        super.display(new KahinaMemTree());
        model.setRootID(0);
        int rootID = model.getRootID();
        model.setNodeCaption(rootID,generateNodeCaption(formula));
        frmToNode.put(formula, rootID);
        nodeToFrm.put(rootID, formula);
        recalculate();
    }
    
    public void toggleFormulaCollapse(int nodeID)
    {
        model.setNodeStatus(nodeID, 1);
        BooleanFormula frm = nodeToFrm.get(nodeID);
        if (frm != null)
        {
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
                
            }
        }
    }
    
    private void addFormulaNode(BooleanFormula f, int parentID)
    {
        int nodeID = model.addNode(generateNodeCaption(f), "", 0);
        frmToNode.put(f, nodeID);
        nodeToFrm.put(nodeID, f);
        model.addChild(parentID, nodeID);
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
            return "and(" + ((Conjunction) f).getFms().size() + ")";
        }
        else if (f instanceof Disjunction)
        {
            return "or(" + ((Disjunction) f).getFms().size() + ")";
        }
        else if (f instanceof Negation)
        {
            return "not";
        }
        else
        {
            return "???";
        }
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
