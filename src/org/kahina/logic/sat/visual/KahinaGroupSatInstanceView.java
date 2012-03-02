package org.kahina.logic.sat.visual;

import java.awt.Color;
import java.util.HashMap;

import org.kahina.core.control.KahinaController;
import org.kahina.core.visual.graph.KahinaGraphLayouter;
import org.kahina.logic.sat.data.GroupCnfSatInstance;

public class KahinaGroupSatInstanceView extends KahinaSatInstanceView
{
    GroupCnfSatInstance sat;
    
    public KahinaGroupSatInstanceView(KahinaController control, KahinaGraphLayouter layout)
    {
        super(control, layout);
    }

    public void display(GroupCnfSatInstance sat)
    {
        //do not recalculate if the sat instance is already displayed
        if (this.sat == null || this.sat != sat)
        {
            this.sat = sat;
            super.sat = sat;
            displayGroupsByVariables();
        }
    }
    
    public void displayGroupsByVariables()
    {
        model = sat.generateClaGroupByVarGraph();
        if (!clauseGraph)
        {
            vertexBorderColor = new HashMap<Integer, Color>();
            resetLayoutStructures();
            layout.newGraph(this);
        }
        clauseGraph = true;
    }
    
    public void displayGroupsByLiterals()
    {
        model = sat.generateClaGroupByLitGraph();
        if (!clauseGraph)
        {
            vertexBorderColor = new HashMap<Integer, Color>();
            resetLayoutStructures();
            layout.newGraph(this);
        }
        clauseGraph = true;
    }
    
    public void displayGroupsByComplementaryLiterals()
    {
        model = sat.generateClaGroupByCompLitGraph();
        if (!clauseGraph)
        {
            vertexBorderColor = new HashMap<Integer, Color>();
            resetLayoutStructures();
            layout.newGraph(this);
        }
        clauseGraph = true;
    }
    
    public void displayVariablesByGroups()
    {
        model = sat.generateVarByClaGroupGraph();
        vertexBorderColor = new HashMap<Integer, Color>();
        resetLayoutStructures();
        layout.newGraph(this);
        clauseGraph = false;
    }
    
    public void displayLiteralsByGroups()
    {
        model = sat.generateLitByClaGroupGraph();
        vertexBorderColor = new HashMap<Integer, Color>();
        resetLayoutStructures();
        layout.newGraph(this);
        clauseGraph = false;
    }
}
