package org.kahina.logic.sat.visual.cnf;

import java.awt.event.ActionListener;

import javax.swing.ButtonGroup;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButtonMenuItem;

import org.kahina.core.visual.graph.KahinaGraphView;
import org.kahina.core.visual.graph.KahinaGraphViewContextMenu;

public class KahinaSatInstanceViewContextMenu extends KahinaGraphViewContextMenu
{
    public KahinaSatInstanceViewContextMenu(ActionListener l, KahinaGraphView v)
    {
        super(l, v);
    }

    public static JPopupMenu getMenu(ActionListener l, KahinaGraphView v)
    {
        return new KahinaSatInstanceViewContextMenu(l, v);
    }
    
    //deriving classes can implement this to extend the context menu
    protected void addAdditionalMenus(ActionListener l)
    {
        JMenu visTypeSubmenu = new JMenu("Choose SAT Visualization");
        
        JMenuItem claByVarItem = new JMenuItem("Clauses connected by variables");
        claByVarItem.setActionCommand("claByVar");
        claByVarItem.addActionListener(l);
        visTypeSubmenu.add(claByVarItem);
        
        JMenuItem claByLitItem = new JMenuItem("Clauses connected by literals");
        claByLitItem.setActionCommand("claByLit");
        claByLitItem.addActionListener(l);
        visTypeSubmenu.add(claByLitItem);
        
        JMenuItem claByCompLitItem = new JMenuItem("Clauses connected by complementary literals");
        claByCompLitItem.setActionCommand("claByCompLit");
        claByCompLitItem.addActionListener(l);
        visTypeSubmenu.add(claByCompLitItem);
        
        JMenuItem varByClaItem = new JMenuItem("Variables connected by clauses");
        varByClaItem.setActionCommand("varByCla");
        varByClaItem.addActionListener(l);
        visTypeSubmenu.add(varByClaItem);
        
        JMenuItem litByVarItem = new JMenuItem("Literals connected by clauses");
        litByVarItem.setActionCommand("litByCla");
        litByVarItem.addActionListener(l);
        visTypeSubmenu.add(litByVarItem);

        add(visTypeSubmenu);   
        addSeparator();
    }
}
