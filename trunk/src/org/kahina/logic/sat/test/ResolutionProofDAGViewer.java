package org.kahina.logic.sat.test;

import java.awt.Color;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;

import org.kahina.core.KahinaDefaultInstance;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.visual.dag.KahinaDAGView;
import org.kahina.core.visual.dag.LayeredLayouter;
import org.kahina.logic.sat.data.proof.ResolutionProofDAG;
import org.kahina.logic.sat.io.proof.ResolutionProofParser;

public class ResolutionProofDAGViewer
{
    public static void main(String[] args)
    {
        if (args.length == 0)
        {
            System.err.println("Usage: java ResolutionProofDAGViewer [proof file]");
            System.exit(1);
        }
        ResolutionProofDAG proof = ResolutionProofParser.createResolutionProofDAG(args[0]);
        
        KahinaDefaultInstance kahina = new KahinaDefaultInstance();
        
        final KahinaDAGView v = new KahinaDAGView(kahina, new LayeredLayouter());
        v.setTitle("Resolution Proof");
        v.getConfig().setVerticalDistance(5);
        v.getConfig().setHorizontalDistance(3);
        v.display(proof);
        v.setStatusColorEncoding(0,new Color(255,255,255));
        v.setStatusColorEncoding(1,new Color(255,0,0));
        v.setStatusColorEncoding(2,new Color(0,255,255));
        v.setStatusColorEncoding(3,new Color(255,255,255)); 
        
        kahina.getGuiControl().registerListener(KahinaEventTypes.SELECTION, v);
        kahina.getGuiControl().registerListener(KahinaEventTypes.UPDATE, v);
        kahina.getGuiControl().registerListener(KahinaEventTypes.REDRAW, v);
        
        SwingUtilities.invokeLater(new Runnable() 
        {
            public void run() 
            {
                JFrame w = new JFrame("Resolution Proof Viewer");
                w.setSize(510, 720);
                w.setLayout(new BoxLayout(w.getContentPane(), BoxLayout.LINE_AXIS));
                w.add(v.makePanel());
                w.setVisible(true);
                w.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);   
            }
        });
    }
}
