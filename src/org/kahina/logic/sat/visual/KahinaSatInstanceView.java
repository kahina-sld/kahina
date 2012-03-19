package org.kahina.logic.sat.visual;

import java.awt.Color;
import java.awt.GridLayout;
import java.util.HashMap;

import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.KahinaProgressBar;
import org.kahina.core.visual.graph.KahinaGraphLayouter;
import org.kahina.core.visual.graph.KahinaGraphView;
import org.kahina.core.visual.graph.KahinaGraphViewPanel;
import org.kahina.logic.sat.data.CnfSatInstance;

public class KahinaSatInstanceView extends KahinaGraphView
{
    CnfSatInstance sat;
    boolean clauseGraph;
    
    public KahinaSatInstanceView(KahinaController control, KahinaGraphLayouter layout)
    {
        super(control, layout);
        clauseGraph = false;
    }
    
    public void display(CnfSatInstance sat)
    {
        //do not recalculate if the sat instance is already displayed
        if (this.sat == null || this.sat != sat)
        {
            this.sat = sat;
            displayClausesByVariables();
        }
    }
    
    public boolean showsClauseGraph()
    {
        return clauseGraph;
    }
    
    public void displayClausesByVariables()
    {
        model = sat.generateClaByVarGraph();
        if (!clauseGraph)
        {
            vertexBorderColor = new HashMap<Integer, Color>();
            resetLayoutStructures();
            layout.newGraph(this);
        }
        clauseGraph = true;
        flushRedrawAgenda();
    }
    
    public void displayClausesByLiterals()
    {
        model = sat.generateClaByLitGraph();
        if (!clauseGraph)
        {
            vertexBorderColor = new HashMap<Integer, Color>();
            resetLayoutStructures();
            layout.newGraph(this);
        }
        clauseGraph = true;
        flushRedrawAgenda();
    }
    
    public void displayClausesByComplementaryLiterals()
    {
        model = sat.generateClaByCompLitGraph();
        if (!clauseGraph)
        {
            vertexBorderColor = new HashMap<Integer, Color>();
            resetLayoutStructures();
            layout.newGraph(this);
        }
        clauseGraph = true;
        flushRedrawAgenda();
    }
    
    public void displayVariablesByClauses()
    {
        model = sat.generateVarByClaGraph();
        vertexBorderColor = new HashMap<Integer, Color>();
        resetLayoutStructures();
        layout.newGraph(this);
        clauseGraph = false;
        flushRedrawAgenda();
    }
    
    public void displayLiteralsByClauses()
    {
        model = sat.generateLitByClaGraph();
        vertexBorderColor = new HashMap<Integer, Color>();
        resetLayoutStructures();
        layout.newGraph(this);
        clauseGraph = false;
        flushRedrawAgenda();
    }
    
    @Override
    public JComponent makePanel()
    {
        KahinaProgressBar progressBar = new KahinaProgressBar();
        KahinaSatInstanceViewPanel panel = new KahinaSatInstanceViewPanel(control);
        control.registerListener("redraw", panel);
        panel.setView(this);
        JScrollPane scrollPane = new JScrollPane(panel);
        scrollPane.getViewport().setBackground(config.getBackgroundColor());
        JPanel scrollPaneAndProgressBar = new JPanel();
        scrollPaneAndProgressBar.setLayout(new BoxLayout(scrollPaneAndProgressBar, BoxLayout.Y_AXIS));
        scrollPaneAndProgressBar.add(scrollPane);
        scrollPaneAndProgressBar.add(progressBar);
        panel.setProgressBar(progressBar);
        return scrollPaneAndProgressBar;
    }
}
