package org.kahina.logic.sat.test;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;

import org.kahina.core.KahinaDefaultInstance;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.io.cnf.DimacsCnfParser;
import org.kahina.logic.sat.visual.cnf.list.KahinaSatInstanceListView;

public class CnfSatInstanceListViewer
{
    public static void main(String[] args)
    {
        if (args.length != 1)
        {
            System.err.println("Usage: java CnfSatInstanceListViewer [cnf file]");
            System.exit(1);
        }
        CnfSatInstance satInstance = DimacsCnfParser.parseDimacsCnfFile(args[0]);
        
        KahinaDefaultInstance kahina = new KahinaDefaultInstance();
        
        final KahinaSatInstanceListView v = new KahinaSatInstanceListView(kahina);
        v.setTitle("SAT Instance view");
        v.display(satInstance);
        
        kahina.registerInstanceListener(KahinaEventTypes.SELECTION, v);
        kahina.registerInstanceListener(KahinaEventTypes.UPDATE, v);
        kahina.registerInstanceListener(KahinaEventTypes.REDRAW, v);
        
        SwingUtilities.invokeLater(new Runnable() 
        {
            public void run() 
            {
                JFrame w = new JFrame("CNF SAT Instance List Viewer");
                w.setSize(510, 720);
                w.setLayout(new BoxLayout(w.getContentPane(), BoxLayout.LINE_AXIS));
                w.add(v.makePanel());
                w.setVisible(true);
                w.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);   
            }
        });
    }
}
