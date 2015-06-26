# xmonad-sessions

Simplified version of [zaxtax/xmonad-sessions](https://github.com/zaxtax/xmonad-sessions) that also saves workspace of the windows to be restored.
xmonad-sessions is a way to use xmonad to control which of your applications persist and are easily restored.

## Installing

place ViewDoc.hs into ~/.xmonad/lib/

<pre><code>
$ cp ViewDoc.hs ~/.xmonad/lib
</code></pre>

modify xmonad.hs to import:
<pre><code>
import ViewDoc
import XMonad.Actions.SpawnOn
</pre></code>

and call <code>toggleSaveState</code> to save/unsave focused window and <code>launchDocuments</code> to restore all saved windows, e.g. bind it to keys:
<pre><code>
mykeys (XConfig {modMask = modm}) = M.fromList $
   [  ((modm, xK_s), toggleSaveState)
   ,  ((modm .|. shiftMask, xK_s), launchDocuments)
   ]
</code></pre>

And use <code>manageSpawn</code> in your manageHooks, e.g.
<pre><code>
main = xmonad $ defaultConfig 
       { managehook = manageSpawn &lt;+&gt; myManageHook
       , ...
       }
</code></pre>


## Using

For a setup as described above:

If you want to save any window focus over it and Mod-s.
Mod-s again will untoggle it

To restore all previous windows saved Mod-S

All applications that have been wrapped with saveDocument will be restorable
