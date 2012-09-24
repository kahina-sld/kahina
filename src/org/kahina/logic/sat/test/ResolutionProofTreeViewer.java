package org.kahina.logic.sat.test;

import java.awt.Color;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;

import org.kahina.core.KahinaDefaultInstance;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.data.tree.KahinaMemTree;
import org.kahina.core.visual.tree.KahinaTreeView;
import org.kahina.core.visual.tree.KahinaTreeViewOptions;
import org.kahina.logic.sat.data.KahinaSatInstance;
import org.kahina.logic.sat.data.proof.ResolutionProofTree;
import org.kahina.logic.sat.io.cnf.DimacsCnfParser;
import org.kahina.logic.sat.io.proof.ResolutionProofParser;

public class ResolutionProofTreeViewer
{
    public static void main(String[] args)
    {
        if (args.length == 0)
        {
            System.err.println("Usage: java ResolutionProofTreeViewer [proof file]");
            System.exit(1);
        }
        KahinaSatInstance satInstance = new KahinaSatInstance();
        if (args.length >= 2)
        {
            satInstance = DimacsCnfParser.parseDimacsCnfFile(args[1]);
        }
        else
        {
            System.err.println("No SAT instance given, generating view without symbols.");
        }
        
        ResolutionProofTree proof = ResolutionProofParser.createResolutionProofTree(args[0], satInstance);
        
        KahinaDefaultInstance kahina = new KahinaDefaultInstance();
        
        final KahinaTreeView v = new KahinaTreeView(kahina);
        v.setTitle("Resolution Proof");
        v.getConfig().setVerticalDistance(20);
        v.getConfig().setHorizontalDistance(15);
        v.getConfig().setNodePositionPolicy(KahinaTreeViewOptions.CENTERED_NODES);
        v.getConfig().setLineShapePolicy(KahinaTreeViewOptions.STRAIGHT_LINES);
        v.getConfig().setEdgeTagPolicy(KahinaTreeViewOptions.NO_EDGE_TAGS);
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
