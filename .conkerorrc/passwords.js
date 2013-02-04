session_pref("signon.rememberSignons", true);
session_pref("signon.expireMasterPassword", false);
session_pref("signon.SignonFileName", "signons.txt");
Cc["@mozilla.org/login-manager;1"].getService(Ci.nsILoginManager); // init

(function() {
    var pwhp_path = "file://" + get_pref("conkeror.rcfile") + "/modules/pwhp-conk";
    if (load_paths.indexOf(pwhp_path) == -1) {
        load_paths.unshift(pwhp_path);
    }
    require("password-hasher-plus.js");
})();
