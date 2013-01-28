define_variable("navigate_on_paste_button", 1,
    "Which mouse button should navigate to the current selection. " +
    "0 = left, 1 = middle, 2 = right. Default is 1.");

define_variable("navigate_on_paste_webjump", "google",
    "Which webjump to use for navigation if the selection is no URL. " +
    "Default is google.");

function navigate_on_paste (event) {
    if (event.button != navigate_on_paste_button)
        return;

    var selection = read_from_x_primary_selection();
    var dest = null;
    if (possibly_valid_url(selection))
        dest = load_spec(selection);
    else
        dest = get_webjump(navigate_on_paste_webjump + " " + selection);

    var window = this.ownerDocument.defaultView;
    var buffer = window.buffers.current;
    buffer.load(dest);
    event.stopPropagation();
}

function navigate_on_paste_add_listener (buffer) {
    if (buffer instanceof content_buffer) {
        buffer.browser.addEventListener("click",
                                        navigate_on_paste,
                                        false);
    }
}

function navigate_on_paste_remove_listener (buffer) {
    buffer.browser.removeEventListener("click",
                                       navigate_on_paste,
                                       false);
}

function navigate_on_paste_mode_enable () {
    add_hook("create_buffer_hook",
             navigate_on_paste_add_listener);
    for_each_buffer(navigate_on_paste_add_listener);
}

function navigate_on_paste_mode_disable () {
    remove_hook("create_buffer_hook",
                navigate_on_paste_add_listener);
    for_each_buffer(navigate_on_paste_remove_listener);
}

define_global_mode("navigate_on_paste_mode",
                   navigate_on_paste_mode_enable,
                   navigate_on_paste_mode_disable);

navigate_on_paste_mode(true);

provide("navigate-on-paste");
