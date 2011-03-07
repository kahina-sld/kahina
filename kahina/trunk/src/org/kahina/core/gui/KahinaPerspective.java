package org.kahina.core.gui;

import java.util.Map;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.visual.KahinaView;
import org.kahina.core.visual.KahinaViewConfiguration;

/**
 * Storage of window configuration and display options for a Kahina instance.
 * <p>
 * Main functionality is to represent sets of configurations.
 * A {@link KahinaWindowManager} can build a Kahina environment or re-arrange it
 * according to the instructions contained in a perspective. 
 * <p>
 * It is the task of the {@link KahinaWindowManager} to react to user-imposed changes
 * of display options and window layout by changing the corresponding setting
 * in the current perspective.
 * <p>
 * A perspective defines one of the important parts of an
 * application profile, and is therefore usually persistent across sessions.
 * 
 * @author jdellert
 *
 */

public class KahinaPerspective 
{
	//views are indexed by string identifiers
	Map<String,KahinaViewConfiguration<KahinaView<?>>> config;
}
