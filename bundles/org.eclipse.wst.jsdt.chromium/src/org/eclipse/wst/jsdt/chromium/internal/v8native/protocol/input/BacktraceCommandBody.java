// Copyright (c) 2009 The Chromium Authors. All rights reserved.
// This program and the accompanying materials are made available
// under the terms of the Eclipse Public License v1.0 which accompanies
// this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html

package org.eclipse.wst.jsdt.chromium.internal.v8native.protocol.input;

import java.util.List;

import org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonOptionalField;
import org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonSubtype;
import org.eclipse.wst.jsdt.chromium.internal.protocolparser.JsonType;

@JsonType
public interface BacktraceCommandBody extends JsonSubtype<CommandResponseBody> {
  @JsonOptionalField
  List<FrameObject> frames();

  @JsonOptionalField
  Long fromFrame();

  @JsonOptionalField
  Long toFrame();

  Long totalFrames();
}
