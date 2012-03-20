package org.kahina.core.test;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.JFrame;
import javax.swing.SwingUtilities;

import org.kahina.core.control.KahinaController;
import org.kahina.core.data.graph.KahinaGraph;
import org.kahina.core.gui.event.KahinaRedrawEvent;
import org.kahina.core.visual.graph.GridLayouter;
import org.kahina.core.visual.graph.KahinaGraphView;
import org.kahina.core.visual.graph.SpringLayouter;

public class KahinaGraphTest
{
    public static void main(String[] args)
    {
        final KahinaController control = new KahinaController();
        
        KahinaGraph graph = KahinaGraph.importTGF("/home/dellert/workspace/Kahina/src/org/kahina/core/test/test-graph.tgf");
        final KahinaGraphView view = new KahinaGraphView(control, new SpringLayouter());
        view.getConfig().setZoomLevel(5);
        view.display(graph);  
        
        SwingUtilities.invokeLater(new Runnable() 
        {
            public void run() 
            {
                JFrame w = new JFrame("KahinaGraphView Demo");
                w.addKeyListener(new OptimizeKeyListener(view, control));
                w.setSize(600, 600);
                w.setLayout(null);
                w.add(view.makePanel());
                w.setVisible(true);
                w.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);   
            }
        });
    }
    
    private static class OptimizeKeyListener implements KeyListener
    {
        KahinaGraphView view;
        KahinaController control;
        
        public OptimizeKeyListener(KahinaGraphView view, KahinaController control)
        {
            this.view = view;
            this.control = control;
        }

        @Override
        public void keyPressed(KeyEvent e)
        {
            // TODO Auto-generated method stub
            
        }

        @Override
        public void keyReleased(KeyEvent e)
        {
            // TODO Auto-generated method stub
            
        }

        @Override
        public void keyTyped(KeyEvent e)
        {
           if (e.getKeyChar() == 'o')
           {
               System.err.println("Optimizing!");
               view.getLayouter().optimize();
               view.flushRedrawAgenda();
               control.processEvent(new KahinaRedrawEvent());
           }       
        }
    }
}
