package org.kahina.core;

import org.kahina.qtype.QTypeDebuggerInstance;
import org.kahina.sicstus.bridge.SICStusPrologBridge;

public class ListTreeViewPrimaryBranchingTest {

	public static void main(String[] args) {
		QTypeDebuggerInstance kahina = new QTypeDebuggerInstance();
		SICStusPrologBridge bridge = kahina.startNewSession();
		bridge.step(0, "[query]");
		bridge.call(0);
		bridge.step(1, "a(X)");
		bridge.call(1);
		bridge.fail(1);
		bridge.step(2, "b");
		bridge.call(2);
		bridge.exit(2, true);
		bridge.exit(0, true);
	}

}
