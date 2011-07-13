package org.kahina.core.breakpoint;

/**
 * A selection of constant symbols defining meaningful values for the <code>type</code> property of a {@link KahinaBreakpoint}.
 * <p>
 * Applications may define additional breakpoint types by inheriting from this class.
 * To avoid the redefinition of existing types, user-defined extensions should not assign the integer values 0 through 8 to any constant.
 * 
 * @author jd
 *
 */
public class KahinaBreakpointType
{
    public static final int PRIMARY_BREAKPOINT = 0;
    public static final int SECONDARY_BREAKPOINT = 1;
    public static final int SKIP_POINT = 2;
    public static final int PROFILE_POINT = 3;
    public static final int CREEP_POINT = 4;
    public static final int FAIL_POINT = 5;
	public static final int PRIMARY_WARN_POINT = 7;
	public static final int SECONDARY_WARN_POINT = 8;
}
