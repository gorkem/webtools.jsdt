// Generated source.
// Generator: org.eclipse.wst.jsdt.chromium.internal.wip.tools.protocolgenerator.Generator
// Origin: http://src.chromium.org/blink/trunk/Source/devtools/protocol.json@<unknown>

package org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.dom;

/**
 DOM interaction is implemented in terms of mirror objects that represent the actual DOM nodes. DOMNode is a base node mirror type.
 */
@org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonType
public interface NodeValue {
  /**
   Node identifier that is passed into the rest of the DOM messages as the <code>nodeId</code>. Backend will only push node with given <code>id</code> once. It is aware of all requested nodes and will only fire DOM events for nodes known to the client.
   */
  long/*See org.eclipse.wst.jsdt.chromium.internal.wip.protocol.common.dom.NodeIdTypedef*/ nodeId();

  /**
   <code>Node</code>'s nodeType.
   */
  long nodeType();

  /**
   <code>Node</code>'s nodeName.
   */
  String nodeName();

  /**
   <code>Node</code>'s localName.
   */
  String localName();

  /**
   <code>Node</code>'s nodeValue.
   */
  String nodeValue();

  /**
   Child count for <code>Container</code> nodes.
   */
  @org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonOptionalField
  Long childNodeCount();

  /**
   Child nodes of this node when requested with children.
   */
  @org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonOptionalField
  java.util.List<org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.dom.NodeValue> children();

  /**
   Attributes of the <code>Element</code> node in the form of flat array <code>[name1, value1, name2, value2]</code>.
   */
  @org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonOptionalField
  java.util.List<String> attributes();

  /**
   Document URL that <code>Document</code> or <code>FrameOwner</code> node points to.
   */
  @org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonOptionalField
  String documentURL();

  /**
   Base URL that <code>Document</code> or <code>FrameOwner</code> node uses for URL completion.
   */
  @org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonOptionalField
  String baseURL();

  /**
   <code>DocumentType</code>'s publicId.
   */
  @org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonOptionalField
  String publicId();

  /**
   <code>DocumentType</code>'s systemId.
   */
  @org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonOptionalField
  String systemId();

  /**
   <code>DocumentType</code>'s internalSubset.
   */
  @org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonOptionalField
  String internalSubset();

  /**
   <code>Document</code>'s XML version in case of XML documents.
   */
  @org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonOptionalField
  String xmlVersion();

  /**
   <code>Attr</code>'s name.
   */
  @org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonOptionalField
  String name();

  /**
   <code>Attr</code>'s value.
   */
  @org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonOptionalField
  String value();

  /**
   Pseudo element type for this node.
   */
  @org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonOptionalField
  org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.dom.PseudoTypeEnum pseudoType();

  /**
   Shadow root type.
   */
  @org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonOptionalField
  org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.dom.ShadowRootTypeEnum shadowRootType();

  /**
   Frame ID for frame owner elements.
   */
  @org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonOptionalField
  String/*See org.eclipse.wst.jsdt.chromium.internal.wip.protocol.common.page.FrameIdTypedef*/ frameId();

  /**
   Content document for frame owner elements.
   */
  @org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonOptionalField
  org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.dom.NodeValue contentDocument();

  /**
   Shadow root list for given element host.
   */
  @org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonOptionalField
  java.util.List<org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.dom.NodeValue> shadowRoots();

  /**
   Content document fragment for template elements.
   */
  @org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonOptionalField
  org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.dom.NodeValue templateContent();

  /**
   Pseudo elements associated with this node.
   */
  @org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonOptionalField
  java.util.List<org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.dom.NodeValue> pseudoElements();

  /**
   Import document for the HTMLImport links.
   */
  @org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonOptionalField
  org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.dom.NodeValue importedDocument();

  /**
   Distributed nodes for given insertion point.
   */
  @org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonOptionalField
  java.util.List<org.eclipse.wst.jsdt.chromium.internal.wip.protocol.input.dom.BackendNodeValue> distributedNodes();

}
