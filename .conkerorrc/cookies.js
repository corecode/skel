// Cookie Culler Stuff
var cookie_culler_chrome = "chrome://cookieculler/content/CookieCuller.xul";

interactive("cookie-culler-dialog", "Show the CookieCuller settings in a dialog box.",
            function (I) {
                var frame = I.buffer.top_frame;
                frame.openDialog(cookie_culler_chrome,
                                 "CookieCuller",
                                 "centerscreen,chrome,dialog,modal,resizable");
            });

interactive("cookie-culler", "Open the CookieCuller settings in a new buffer.",
            "find-url-new-buffer",
            $browser_object = cookie_culler_chrome);
