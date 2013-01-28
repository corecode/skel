session_pref("signon.rememberSignons", true);
session_pref("signon.expireMasterPassword", false);
session_pref("signon.SignonFileName", "signons.txt");
Cc["@mozilla.org/login-manager;1"].getService(Ci.nsILoginManager); // init
