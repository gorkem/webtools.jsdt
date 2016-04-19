/*
 * Copyright 2015 The Closure Compiler Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.google.debugging.sourcemap;

import com.google.debugging.sourcemap.proto.Mapping.OriginalMapping;

/** GWT compatible no-op replacement of {@code SourceMapConsumerV3} */
public final class SourceMapConsumerV3 {
  public OriginalMapping getMappingForLine(int lineNumber, int column) {
    throw new UnsupportedOperationException(
        "SourceMapConsumerV3.getMappingForLine not implemented");
  }

  public void parse(String contents) throws SourceMapParseException {
  }
}
