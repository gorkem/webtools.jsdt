// Generated source.
// Generator: org.eclipse.wst.jsdt.chromium.internal.wip.tools.protocolgenerator.Generator
// Origin: http://src.chromium.org/blink/trunk/Source/devtools/protocol.json@<unknown>

package org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.dom;

/**
 Fired when backend wants to provide client with the missing DOM structure. This happens upon most of the calls requesting node ids.
 */
@org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonType
public interface SetChildNodesEventData {
  /**
   Parent node id to populate with children.
   */
  long/*See org.eclipse.wst.jsdt.chromium.internal.wip.protocol.common.dom.NodeIdTypedef*/ parentId();

  /**
   Child nodes array.
   */
  java.util.List<org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.dom.NodeValue> nodes();

  public static final org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.WipEventType<org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.dom.SetChildNodesEventData> TYPE
      = new org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.WipEventType<org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.dom.SetChildNodesEventData>("DOM.setChildNodes", org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.dom.SetChildNodesEventData.class) {
    @Override public org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.dom.SetChildNodesEventData parse(org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.WipGeneratedParserRoot parser, org.json.simple.JSONObject obj) throws org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonProtocolParseException {
      return parser.parseDOMSetChildNodesEventData(obj);
    }
  };
}
