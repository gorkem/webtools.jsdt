// Generated source.
// Generator: org.eclipse.wst.jsdt.chromium.internal.wip.tools.protocolgenerator.Generator
// Origin: http://svn.webkit.org/repository/webkit/!svn/bc/92284/trunk/Source/WebCore/inspector/Inspector.json@92284

package org.eclipse.wst.jsdt.chromium.internal.wip.protocol.output.debugger;

/**
Sets JavaScript breakpoint at a given location.
 */
public class SetBreakpointParams extends org.eclipse.wst.jsdt.chromium.internal.wip.protocol.output.WipParamsWithResponse<org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.debugger.SetBreakpointData> {
  /**
   @param location Location to set breakpoint in.
   @param conditionOpt Expression to use as a breakpoint condition. When specified, debugger will only stop on the breakpoint if this expression evaluates to true.
   */
  public SetBreakpointParams(org.eclipse.wst.jsdt.chromium.internal.wip.protocol.output.debugger.LocationParam location, String conditionOpt) {
    this.put("location", location);
    if (conditionOpt != null) {
      this.put("condition", conditionOpt);
    }
  }

  public static final String METHOD_NAME = org.eclipse.wst.jsdt.chromium.internal.wip.protocol.BasicConstants.Domain.DEBUGGER + ".setBreakpoint";

  @Override protected String getRequestName() {
    return METHOD_NAME;
  }

  @Override public org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.debugger.SetBreakpointData parseResponse(org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.WipCommandResponse.Data data, org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.WipGeneratedParserRoot parser) throws org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonProtocolParseException {
    return parser.parseDebuggerSetBreakpointData(data.getUnderlyingObject());
  }

}
