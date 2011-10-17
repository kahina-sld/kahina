package org.kahina.tralesld.visual.workbench;

import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JList;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.DefaultListModel;

import org.kahina.core.KahinaRunner;
import org.kahina.core.visual.KahinaViewPanel;
import org.kahina.tralesld.TraleSLDState;
import org.kahina.tralesld.data.FeatureWorkbench;
import org.kahina.tralesld.data.fs.TraleSLDFS;
import org.kahina.tralesld.data.fs.TraleSLDPackedFSTerminal;
import org.kahina.tralesld.visual.fs.TraleSLDFeatureStructureEditor;
import org.kahina.tralesld.visual.fs.TraleSLDFeatureStructureView;

/**
 * A feature workbench window, with list of objects on the left, 
 * a toolbar with functionality such as unification and diff, 
 * and an editing component on the right.
 * 
 * @author jdellert
 *
 */
public class FeatureWorkbenchViewPanel extends KahinaViewPanel<FeatureWorkbenchView> implements ListSelectionListener
{
	private final JLabel msgLabel;
	private final JList list;
	private final TraleSLDFeatureStructureEditor editor;
	private final FeatureStructureListModel listModel;
	
	public FeatureWorkbenchViewPanel(TraleSLDState state)
	{
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		
		JPanel controlPanel = new JPanel();
		controlPanel.setLayout(new BoxLayout(controlPanel, BoxLayout.X_AXIS));
		
		String[] dummyEntries = { "lexicon", "entry1", "entry2", "entry3" };
		
		JComboBox lexBox = new JComboBox(dummyEntries);
		controlPanel.add(lexBox);
		
		JButton diffButton = new JButton("diff");
		controlPanel.add(diffButton);
		
		JButton mgsButton = new JButton("satisfy_the");
		controlPanel.add(mgsButton);
		
		JButton mguSigButton = new JButton("unify_sig");
		controlPanel.add(mguSigButton);
		
		JButton mguTheButton = new JButton("unify_the");
		controlPanel.add(mguTheButton);
		
		msgLabel = new JLabel("Drag feature structures into this window.");
		controlPanel.add(msgLabel);
		
		add(controlPanel);
		
		JPanel contentPanel = new JPanel();
		contentPanel.setLayout(new BoxLayout(contentPanel, BoxLayout.X_AXIS));
		
		listModel = new FeatureStructureListModel(this.view);
		list = new JList(listModel);
		list.getSelectionModel().addListSelectionListener(this);
		JScrollPane tableScrollPane = new JScrollPane(list);
		contentPanel.add(tableScrollPane);
		
		editor = new TraleSLDFeatureStructureEditor(state);
		JScrollPane editorScrollPane = new JScrollPane(editor);
		contentPanel.add(editorScrollPane);
		
		add(contentPanel);
	}
	
	//TODO: override display method to adapt list model

	@Override
	public void updateDisplay() 
	{

	}

	@Override
	public void valueChanged(ListSelectionEvent arg0) 
	{
		editor.loadGrisu(listModel.getElementAt(arg0.getFirstIndex()));	
		editor.updateDisplay();
	}
	
	private class FeatureStructureListModel extends DefaultListModel
	{
		FeatureWorkbenchView workbenchView;
		
		public FeatureStructureListModel(FeatureWorkbenchView workbench)
		{
			this.workbenchView = workbenchView;
		}
		
		public int getSize()
		{
			return 1;
		}
		
		public String getElementAt(int i)
		{
			//return "!newdata \"cruel\" (S1(0\"mgsat\"))(T2 \"head_subject:cruel\" 1)\n";
			return "!newdata\"Edge\"(S1(0\"word\")(V2\"phon\"(L3(S5(4\"cruel\"))))(V6\"qstore\"(S8(7\"list\")))(V9\"synsem\"(S11(10\"synsem\")(V12\"loc\"(S14(13\"loc\")(V15\"cat\"(S17(16\"cat\")(V18\"determined\"(S20(19\"boolean\")))(V21\"head\"(S23(22\"adj\")(V24\"mod\"(S26(25\"synsem\")(V27\"loc\"(S29(28\"loc\")(V30\"cat\"(S32(31\"cat\")(V33\"determined\"(S35(34\"minus\")))(V36\"head\"(S38(37\"noun\")(V39\"case\"(S41(40\"case\")))(V42\"mod\"(S44(43\"synsem_none\")))(V45\"pred\"(S47(46\"boolean\")))))(V48\"val\"(S50(49\"mgsat(val)\")))))(V51\"cont\"(S53(52\"nom_obj\")(V54\"index\"(#55 1))(V56\"restr\"(#57 2))))))(V58\"nonloc\"(S60(59\"mgsat(nonloc)\")))))(V61\"pred\"(S63(62\"minus\")))))(V64\"val\"(S66(65\"val\")(V67\"subj\"(L68))(V69\"comps\"(#70 0))))))(V71\"cont\"(S73(72\"nom_obj\")(V74\"index\"(#75 1))(V76\"restr\"(L77(S79(78\"psoa\")(V80\"nucleus\"(S82(81\"adjmod\")(V83\"inst\"(#84 1))(V85\"relationname\"(S87(86\"bot\")))(V88\"soa_arg\"(S90(89\"mgsat(psoa)\")))))(V91\"quants\"(L92)))(Z93(#94 2))))))))(V95\"nonloc\"(S97(96\"mgsat(nonloc)\")))))(V98\"arg_st\"(#99 0)))(R100 2(S2(101\"list\")))(R102 0(L0))(R103 1(S1(104\"index\")(V105\"gen\"(S107(106\"gen\")))(V108\"num\"(S110(109\"num\")))(V111\"pers\"(S113(112\"pers\")))(V114\"sort\"(S116(115\"bot\")))))\n";
		}
	}

}
