package org.eclipse.wst.jsdt.chromium.internal.v8native;

import java.util.List;

import org.eclipse.wst.jsdt.chromium.Script;
import org.eclipse.wst.jsdt.chromium.internal.v8native.ScriptManager;
import org.eclipse.wst.jsdt.chromium.internal.v8native.V8ContextFilter;
import org.eclipse.wst.jsdt.chromium.internal.v8native.protocol.input.data.ScriptHandle;
import org.eclipse.wst.jsdt.chromium.internal.v8native.protocol.input.data.SomeHandle;


public class ScriptImplTestGate {

  public static ScriptManager create(V8ContextFilter contextFilter) {
    return new ScriptManager(contextFilter, null);
  }

  public static Script addScript(ScriptManager scriptManager, ScriptHandle scriptBody,
      List<SomeHandle> refs) {
    return scriptManager.addScriptImpl(scriptBody, refs);
  }

}
