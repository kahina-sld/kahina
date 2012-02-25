package org.kahina.logic.sat.visual;

import java.awt.Color;
import java.util.HashMap;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.visual.graph.KahinaGraphLayouter;
import org.kahina.core.visual.graph.KahinaGraphView;
import org.kahina.core.visual.graph.KahinaGraphViewPanel;
import org.kahina.logic.sat.data.CnfSatInstance;

public class KahinaSatInstanceView extends KahinaGraphView
{
    CnfSatInstance sat;
    
    public KahinaSatInstanceView(KahinaController control, KahinaGraphLayouter layout)
    {
        super(control, layout);
    }
    
    public void display(CnfSatInstance sat)
    {
        this.sat = sat;
        displayClausesByVariables();
    }
    
    public void displayClausesByVariables()
    {
        model = sat.generateClaByVarGraph();
        vertexBorderColor = new HashMap<Integer, Color>();
        resetLayoutStructures();
        layout.newGraph(this);
    }
    
    public void displayClausesByLiterals()
    {
        model = sat.generateClaByLitGraph();
        vertexBorderColor = new HashMap<Integer, Color>();
        resetLayoutStructures();
        layout.newGraph(this);
    }
    
    public void displayClausesByComplementaryLiterals()
    {
        model = sat.generateClaByCompLitGraph();
        vertexBorderColor = new HashMap<Integer, Color>();
        resetLayoutStructures();
        layout.newGraph(this);
    }
    
    public void displayVariablesByClauses()
    {
        model = sat.generateVarByClaGraph();
        vertexBorderColor = new HashMap<Integer, Color>();
        resetLayoutStructures();
        layout.newGraph(this);
    }
    
    public void displayLiteralsByClauses()
    {
        model = sat.generateLitByClaGraph();
        vertexBorderColor = new HashMap<Integer, Color>();
        resetLayoutStructures();
        layout.newGraph(this);
    }
    
    @Override
    public JComponent makePanel(KahinaGUI gui)
    {
        KahinaSatInstanceViewPanel panel = new KahinaSatInstanceViewPanel(control);
        control.registerListener("redraw", panel);
        panel.setView(this);
        JScrollPane scrollPane = new JScrollPane(panel);
        scrollPane.getViewport().setBackground(config.getBackgroundColor());
        return scrollPane;
    }
}
