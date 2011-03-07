package org.kahina.core.visual;

import java.util.Map;

import org.kahina.core.data.KahinaObject;
import org.kahina.core.event.KahinaEvent;

/**
 * The generic ancestor class of all view configuration classes.
 * <p>
 * Implementations of this class serve as stores for those
 * properties of a view component that can be manipulated by the user.
 * Implementations of this class should contain all the data necessary
 * to reconstruct a view exactly as it was when restoring a session.
 * <p>
 * This class is generic and can be specialized for any subclass of {@link KahinaView}.
 * @author jdellert
 *
 */
public class KahinaViewConfiguration<T extends KahinaView<?>>  
{
}
