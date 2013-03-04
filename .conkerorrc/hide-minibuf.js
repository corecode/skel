var minibuffer_autohide_message_timeout = 3000;
var minibuffer_autohide_timer = null;
var minibuffer_mutually_exclusive_with_mode_line = true;

var old_minibuffer_restore_state = (old_minibuffer_restore_state ||
                                    minibuffer.prototype._restore_state);
var old_minibuffer_show = (old_minibuffer_show ||
                           minibuffer.prototype.show);
var old_minibuffer_clear = (old_minibuffer_clear ||
                            minibuffer.prototype.clear);

show_minibuffer = function (window) {
    window.minibuffer.element.collapsed = false;
    if (minibuffer_mutually_exclusive_with_mode_line && window.mode_line)
        window.mode_line.container.collapsed = true;
};

hide_minibuffer = function (window) {
    window.minibuffer.element.collapsed = true;
    if (minibuffer_mutually_exclusive_with_mode_line && window.mode_line)
        window.mode_line.container.collapsed = false;
};

minibuffer.prototype._restore_state = function () {
    if (minibuffer_autohide_timer) {
        timer_cancel(minibuffer_autohide_timer);
        minibuffer_autohide_timer = null;
    }
    if (this.current_state)
        this.show();
    else
        hide_minibuffer(this.window);
    old_minibuffer_restore_state.call(this);
};

minibuffer.prototype.hide = function () {
    hide_minibuffer(this.window);
};

minibuffer.prototype.maybe_hide = function () {
    if (!this.active) {
        this.hide();
    } else {
        var self = this;
        if (minibuffer_autohide_timer)
            timer_cancel(minibuffer_autohide_timer);
        minibuffer_autohide_timer = call_after_timeout(
            function (I) {self.maybe_hide();}, minibuffer_autohide_message_timeout);
    }
};

minibuffer.prototype.show = function (str, force, hide_after_timeout) {
    var w = this.window;
    var self = this;
    show_minibuffer(this.window);
    old_minibuffer_show.call(this, str, force);
    if (minibuffer_autohide_timer)
        timer_cancel(minibuffer_autohide_timer);
    if (hide_after_timeout || hide_after_timeout == null) {
        minibuffer_autohide_timer = call_after_timeout(
            function (I) {self.maybe_hide();}, minibuffer_autohide_message_timeout);
    }
};

minibuffer.prototype.clear = function () {
    if (minibuffer_autohide_timer) {
        timer_cancel(minibuffer_autohide_timer);
        minibuffer_autohide_timer = null;
    }
    if (!this.current_state)
        this.hide();
    old_minibuffer_clear.call(this);
};

// make minibuffer and mode-line the same height so that switching
// between them won't trigger rendering.
register_user_stylesheet(make_css_data_uri(
    ["#minibuffer, hbox.mode-line { height: 1.5em !important; "]));

add_hook("window_initialize_hook", function (I) {I.window.minibuffer.hide();});
