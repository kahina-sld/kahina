package org.kahina.core.test;

import java.awt.Color;
import java.util.HashMap;
import java.util.Map;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;

import org.kahina.core.KahinaDefaultInstance;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.gui.KahinaPerspective;
import org.kahina.core.gui.KahinaWindowManager;
import org.kahina.core.gui.windows.KahinaDefaultWindow;
import org.kahina.core.gui.windows.KahinaWindow;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.source.KahinaJEditSourceCodeView;

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
        
        final KahinaDefaultInstance kahina = new KahinaDefaultInstance();
        
        final KahinaJEditSourceCodeView v = new KahinaJEditSourceCodeView(kahina);
        v.setTitle("Kahina Source Code View");
        v.display(sourceCodeLocation);
        
        final KahinaWindowManager wm = new KahinaWindowManager(kahina);
        Map<String, KahinaView<? extends KahinaObject>> nameToView = new HashMap<String, KahinaView<? extends KahinaObject>>();
        nameToView.put("Source Code", v);
        wm.setPerspective(KahinaPerspective.generateDefaultPerspective(nameToView));
        
        kahina.registerInstanceListener(KahinaEventTypes.SELECTION, v);
        kahina.registerInstanceListener(KahinaEventTypes.UPDATE, v);
        kahina.registerInstanceListener(KahinaEventTypes.REDRAW, v);
        
        SwingUtilities.invokeLater(new Runnable() 
        {
            public void run() 
            {
                KahinaWindow w = new KahinaDefaultWindow(v, wm, kahina);
                w.setScrollable(true);
                w.setSize(510, 720);
                w.setLayout(new BoxLayout(w.getContentPane(), BoxLayout.LINE_AXIS));
                w.setVisible(true);
                w.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);   
            }
        });
    }
}
