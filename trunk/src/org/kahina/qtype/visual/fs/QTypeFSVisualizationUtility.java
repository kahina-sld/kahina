package org.kahina.qtype.visual.fs;

import gralej.Config;

import org.kahina.tralesld.visual.fs.FSVisualizationUtility;

public class QTypeFSVisualizationUtility extends FSVisualizationUtility
{
	
	@Override
	protected void applyAdditionalConfigSettings(Config config)
	{
		// QType parse trees do not (currently) contain feature structures, so
		// we only portray types. These in turn add no information to the
		// node labels, so we never need to show them.
		config.set("behavior.nodeContentInitiallyVisible", false);
	}

}
