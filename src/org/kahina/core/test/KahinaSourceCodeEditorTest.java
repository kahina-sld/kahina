package org.kahina.core.test;

import java.awt.Color;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;

import org.kahina.core.KahinaDefaultInstance;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.visual.source.KahinaJEditSourceCodeView;
import org.kahina.core.visual.tree.KahinaTreeView;
import org.kahina.core.visual.tree.KahinaTreeViewOptions;
import org.kahina.logic.sat.data.KahinaSatInstance;
import org.kahina.logic.sat.data.proof.ResolutionProofTree;
import org.kahina.logic.sat.io.cnf.DimacsCnfParser;
import org.kahina.logic.sat.io.proof.ResolutionProofParser;

public class KahinaSourceCodeEditorTest
{
    public static void main(String[] args)
    {
        if (args.length == 0)
        {
            System.err.println("Usage: java KahinaSourceCodeEditorTest [source file]");
            System.exit(1);
        }
        KahinaSourceCodeLocation sourceCodeLocation = new KahinaSourceCodeLocation(args[0], 1);
        
        KahinaDefaultInstance kahina = new KahinaDefaultInstance();
        
        final KahinaJEditSourceCodeView v = new KahinaJEditSourceCodeView(kahina);
        v.setTitle("Kahina Source Code View");
        v.display(sourceCodeLocation);
        
        kahina.registerInstanceListener(KahinaEventTypes.SELECTION, v);
        kahina.registerInstanceListener(KahinaEventTypes.UPDATE, v);
        kahina.registerInstanceListener(KahinaEventTypes.REDRAW, v);
        
        SwingUtilities.invokeLater(new Runnable() 
        {
            public void run() 
            {
                JFrame w = new JFrame("Source Code Viewer");
                w.setSize(510, 720);
                w.setLayout(new BoxLayout(w.getContentPane(), BoxLayout.LINE_AXIS));
                w.add(v.makePanel());
                w.setVisible(true);
                w.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);   
            }
        });
    }
}
