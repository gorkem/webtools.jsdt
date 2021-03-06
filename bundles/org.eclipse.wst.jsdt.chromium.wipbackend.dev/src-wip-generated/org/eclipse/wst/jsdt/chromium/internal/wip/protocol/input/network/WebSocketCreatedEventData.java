// Generated source.
// Generator: org.eclipse.wst.jsdt.chromium.internal.wip.tools.protocolgenerator.Generator
// Origin: http://src.chromium.org/blink/trunk/Source/devtools/protocol.json@<unknown>

package org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.network;

/**
 Fired upon WebSocket creation.
 */
@org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonType
public interface WebSocketCreatedEventData {
  /**
   Request identifier.
   */
  String/*See org.eclipse.wst.jsdt.chromium.internal.wip.protocol.common.network.RequestIdTypedef*/ requestId();

  /**
   WebSocket request URL.
   */
  String url();

  public static final org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.WipEventType<org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.network.WebSocketCreatedEventData> TYPE
      = new org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.WipEventType<org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.network.WebSocketCreatedEventData>("Network.webSocketCreated", org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.network.WebSocketCreatedEventData.class) {
    @Override public org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.network.WebSocketCreatedEventData parse(org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.WipGeneratedParserRoot parser, org.json.simple.JSONObject obj) throws org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonProtocolParseException {
      return parser.parseNetworkWebSocketCreatedEventData(obj);
    }
  };
}
