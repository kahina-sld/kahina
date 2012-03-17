package org.kahina.core.test;

import javax.swing.JFrame;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.graph.KahinaGraph;
import org.kahina.core.visual.graph.GridLayouter;
import org.kahina.core.visual.graph.KahinaGraphView;

public class KahinaGraphTest
{
    public static void main(String[] args)
    {
        KahinaController control = new KahinaController();
        
        KahinaGraph graph = KahinaGraph.importTGF("src/org/kahina/core/test/test-graph.tgf");
        KahinaGraphView view = new KahinaGraphView(control, new GridLayouter());
        view.display(graph);       
        
        JFrame w = new JFrame("KahinaGraphView Demo");
        w.setSize(510, 330);
        w.setLayout(null);
        w.add(view.makePanel());
        w.setVisible(true);
        w.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);        
    }
}
