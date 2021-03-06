// Generated source.
// Generator: org.eclipse.wst.jsdt.chromium.internal.wip.tools.protocolgenerator.Generator
// Origin: http://src.chromium.org/blink/trunk/Source/devtools/protocol.json@<unknown>

package org.eclipse.wst.jsdt.chromium.internal.wip.protocol.output.dom;

/**
Highlights owner element of the frame with given id.
 */
public class HighlightFrameParams extends org.eclipse.wst.jsdt.chromium.internal.wip.protocol.output.WipParams {
  /**
   @param frameId Identifier of the frame to highlight.
   @param contentColorOpt The content box highlight fill color (default: transparent).
   @param contentOutlineColorOpt The content box highlight outline color (default: transparent).
   */
  public HighlightFrameParams(String/*See org.eclipse.wst.jsdt.chromium.internal.wip.protocol.common.page.FrameIdTypedef*/ frameId, org.eclipse.wst.jsdt.chromium.internal.wip.protocol.output.dom.RGBAParam contentColorOpt, org.eclipse.wst.jsdt.chromium.internal.wip.protocol.output.dom.RGBAParam contentOutlineColorOpt) {
    this.put("frameId", frameId);
    if (contentColorOpt != null) {
      this.put("contentColor", contentColorOpt);
    }
    if (contentOutlineColorOpt != null) {
      this.put("contentOutlineColor", contentOutlineColorOpt);
    }
  }

  public static final String METHOD_NAME = org.eclipse.wst.jsdt.chromium.internal.wip.protocol.BasicConstants.Domain.DOM + ".highlightFrame";

  @Override protected String getRequestName() {
    return METHOD_NAME;
  }

}
