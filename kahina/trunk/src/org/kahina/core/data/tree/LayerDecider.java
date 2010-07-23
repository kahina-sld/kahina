package org.kahina.core.data.tree;

import java.io.Serializable;

import org.kahina.core.data.KahinaObject;

public abstract class LayerDecider extends KahinaObject implements Serializable
{
	/**
	 * 
	 */
	private static final long serialVersionUID = 4289330877894493409L;

	public abstract int decideOnLayer(int nodeID, KahinaTree tree);
}
