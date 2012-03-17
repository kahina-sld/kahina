package org.kahina.core.test;

import javax.swing.JFrame;
import javax.swing.JScrollPane;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.graph.KahinaGraph;
import org.kahina.core.visual.graph.GridLayouter;
import org.kahina.core.visual.graph.KahinaGraphView;
import org.kahina.core.visual.graph.KahinaGraphViewPanel;

public class KahinaGraphTest
{
    public static void main(String[] args)
    {
        KahinaController control = new KahinaController();
        
        KahinaGraph graph = KahinaGraph.importTGF("src/org/kahina/core/test/test-graph.tgf");
        KahinaGraphView view = new KahinaGraphView(control, new GridLayouter());
        view.display(graph);       

        KahinaGraphViewPanel vp = new KahinaGraphViewPanel(control);
        JScrollPane vpp = new JScrollPane(vp);
        vpp.setBounds(0, 0, 500, 300);
        JFrame w = new JFrame("KahinaGraphView Demo");
        w.setSize(510, 330);
        w.setLayout(null);
        w.add(vpp);
        w.setVisible(true);
        w.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        w.setResizable(false);
        vp.setView(view);        
    }
}
