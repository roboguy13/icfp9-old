import Graphics.UI.Gtk.Misc.Viewport

example :: ViewportClass a => a -> IO Adjustment
example x = viewportGetVAdjustment x
